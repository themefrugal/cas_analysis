from __future__ import annotations

import io
import re
from dataclasses import dataclass
from datetime import date
from typing import BinaryIO, Iterable

import pandas as pd


TRANSACTION_PATTERN = re.compile(
    r"^(\d{2}-[A-Za-z]{3}-\d{4})\s+"
    r"([()\d,.]+)\s+"
    r"([\d,.]+)\s+"
    r"([()\d,.]+)\s+"
    r"(.*?)\s+"
    r"([\d,.]+)\s*$"
)
R_TRANSACTION_PATTERN = re.compile(
    r"^(\d{2}-[A-Za-z]{3}-\d{4})\s+"
    r"(.*?)\s+"
    r"([()\d,.]+)\s+"
    r"([()\d,.]+)\s+"
    r"([\d,.]+)\s+"
    r"([\d,.]+)\s*$"
)
NAV_LINE_PATTERN = re.compile(
    r"NAV on (\d{2}-[A-Za-z]{3}-\d{4}):\s+INR\s+([\d,.]+)"
    r"\s+Valuation on \d{2}-[A-Za-z]{3}-\d{4}:\s+INR\s+([\d,.]+)"
)
CLOSING_BAL_PATTERN = re.compile(r"Closing Unit Balance:\s+([\d,.]+)")
CLOSING_INLINE_PATTERN = re.compile(
    r"Closing Unit Balance:\s+([\d,.]+)\s+NAV:\s+INR\s+([\d,.]+)\s+"
    r"Market Value on\s+(\d{2}-[A-Za-z]{3}-\d{4}):\s+INR\s+([\d,.]+)"
)
IDCW_PATTERN = re.compile(r"^(\d{2}-[A-Za-z]{3}-\d{4})\s+\*+(.*)\*+\s+([,.0-9]+)")
DATE_PREFIX_PATTERN = re.compile(r"^\d{2}-[A-Za-z]{3}-\d{4}\b")
FUND_NAME_PATTERN = re.compile(r"^[A-Z0-9]+\s*-\s*[A-Za-z&]")
FUND_ADVISOR_PATTERN = re.compile(r"(.+)\(Advisor:\s+(.*)\)")
FOLIO_PAN_PATTERN = re.compile(r"\s+PAN:\s+")
EMPTY_TXN_COLUMNS = [
    "date",
    "description",
    "amt",
    "units",
    "nav",
    "bal_units",
    "days",
    "years",
    "amc",
    "fund",
    "advisor",
    "folio",
    "pan",
]


class CasParseError(ValueError):
    """Raised when a CAS PDF cannot be parsed into the expected structure."""


@dataclass(frozen=True)
class CasState:
    all_lines: list[str]
    folio_lines: list[int]
    amc_lines: list[int]
    opening_lines: list[int]
    closing_lines: list[int]


def pdf_text(file: str | BinaryIO, password: str = "") -> list[str]:
    candidates = _pdf_text_candidates(file, password)
    if not candidates:
        raise CasParseError("No readable text was found in the PDF.")
    return candidates[0][1]


def _read_pdf_bytes(file: str | BinaryIO) -> bytes:
    if isinstance(file, str):
        with open(file, "rb") as handle:
            return handle.read()
    pos = None
    if hasattr(file, "tell") and hasattr(file, "seek"):
        try:
            pos = file.tell()
        except Exception:
            pos = None
    data = file.read()
    if pos is not None:
        try:
            file.seek(pos)
        except Exception:
            pass
    return data


def _pdf_text_candidates(file: str | BinaryIO, password: str = "") -> list[tuple[str, list[str]]]:
    pdf_bytes = _read_pdf_bytes(file)
    candidates: list[tuple[str, list[str]]] = []

    try:
        from pypdf import PdfReader

        reader = PdfReader(io.BytesIO(pdf_bytes))
        if reader.is_encrypted:
            result = reader.decrypt(password or "")
            if result == 0:
                raise CasParseError("The PDF password is incorrect. Please correct it and analyze again.")
        pages = [page.extract_text() or "" for page in reader.pages]
        if any(page.strip() for page in pages):
            candidates.append(("pypdf", pages))
    except ImportError:
        pass

    try:
        import fitz

        doc = fitz.open(stream=pdf_bytes, filetype="pdf")
        if doc.needs_pass and not doc.authenticate(password or ""):
            raise CasParseError("The PDF password is incorrect. Please correct it and analyze again.")
        pages = [page.get_text("text") or "" for page in doc]
        doc.close()
        if any(page.strip() for page in pages):
            candidates.append(("pymupdf-text", pages))
    except ImportError:
        pass

    return candidates


def cas_state_from_pages(pages: Iterable[str]) -> CasState:
    lines: list[str] = []
    for page in pages:
        lines.extend(re.sub(r"[\u2212\u2010\u2011\u2012\u2013\u2014]", "-", page).splitlines())

    state = CasState(
        all_lines=lines,
        folio_lines=[i for i, line in enumerate(lines) if re.search(r"Folio No\s*:", line, re.I)],
        amc_lines=[i for i, line in enumerate(lines) if re.search(r"Mutual Fund", line, re.I)],
        opening_lines=[i for i, line in enumerate(lines) if re.search(r"Opening Unit Balance:", line, re.I)],
        closing_lines=[i for i, line in enumerate(lines) if re.search(r"Closing Unit Balance:", line, re.I)],
    )
    validate_cas_state(state)
    return state


def parse_cas_pdf(file: str | BinaryIO, password: str = "") -> CasState:
    candidates = _pdf_text_candidates(file, password)
    if not candidates:
        raise CasParseError("No readable text was found in the PDF.")

    errors = []
    for label, pages in candidates:
        try:
            state = cas_state_from_pages(pages)
            get_portfolio_transactions(state)
            return state
        except CasParseError as exc:
            errors.append(f"{label}: {exc}")

    if errors:
        raise CasParseError("Could not parse transactions from the PDF text. " + " | ".join(errors))
    raise CasParseError("Could not parse transactions from the PDF text.")


def validate_cas_state(state: CasState) -> None:
    n_folio = len(state.folio_lines)
    n_open = len(state.opening_lines)
    n_close = len(state.closing_lines)
    if n_folio == 0:
        raise CasParseError("No folio sections were found in the CAS PDF.")
    if n_folio != n_open or n_folio != n_close:
        raise CasParseError(
            "CAS structure mismatch: found "
            f"{n_folio} folio lines, {n_open} opening balance lines, and {n_close} closing balance lines."
        )
    for ordinal, (folio, opening, closing) in enumerate(
        zip(state.folio_lines, state.opening_lines, state.closing_lines), start=1
    ):
        if not folio < opening < closing:
            raise CasParseError(f"CAS structure mismatch around folio ordinal {ordinal}.")


def _parse_num(value: str) -> float:
    clean = value.strip().replace(",", "")
    if clean.startswith("(") and clean.endswith(")"):
        return -float(clean[1:-1])
    return float(clean)


def _empty_transactions() -> pd.DataFrame:
    return pd.DataFrame(columns=EMPTY_TXN_COLUMNS)


def fund_and_advisor(folio_ord_num: int, state: CasState) -> tuple[str, str]:
    folio_idx = state.folio_lines[folio_ord_num]
    opening_idx = state.opening_lines[folio_ord_num]
    candidates: list[str] = state.all_lines[folio_idx : opening_idx + 1]
    if folio_idx > 0:
        candidates.append(state.all_lines[folio_idx - 1])
    if folio_idx > 1:
        candidates.append(state.all_lines[folio_idx - 2])

    fund_line = next(
        (
            line
            for line in candidates
            if FUND_NAME_PATTERN.search(line.strip()) and not line.strip().upper().startswith("CAMSCASWS")
        ),
        "",
    )
    fund_line = re.sub(r"\s*Registrar\s*:.*", "", fund_line, flags=re.I)
    fund_line = re.split(r"\s{6,}", fund_line)[0].strip()
    match = FUND_ADVISOR_PATTERN.match(fund_line)
    if match:
        fund, advisor = match.groups()
        fund, advisor = fund.strip(), advisor.strip()
    else:
        fund, advisor = fund_line, ""

    if fund and not re.search(r"INF[A-Z0-9]{9}", fund, re.I):
        isin_line = next((line for line in candidates if re.search(r"ISIN:\s*INF[A-Z0-9]{9}", line, re.I)), "")
        isin = re.search(r"INF[A-Z0-9]{9}", isin_line, re.I)
        if isin:
            fund = f"{fund} - ISIN: {isin.group(0).upper()}"
    if not fund:
        raise CasParseError(f"No fund-name line found for folio ordinal {folio_ord_num + 1}.")
    return fund, advisor


def folio_and_pan(folio_ord_num: int, state: CasState) -> tuple[str, str]:
    line = state.all_lines[state.folio_lines[folio_ord_num]]
    parts = FOLIO_PAN_PATTERN.split(line)
    if len(parts) < 2:
        raise CasParseError(f"No PAN marker found for folio ordinal {folio_ord_num + 1}.")
    folio = re.split(r"Folio No:\s+", parts[0])[-1].strip()
    return folio, parts[1][:10].strip()


def _closing_row(folio_ord_num: int, state: CasState) -> dict | None:
    closing_idx = state.closing_lines[folio_ord_num]
    closing_line = state.all_lines[closing_idx]
    inline_match = CLOSING_INLINE_PATTERN.search(closing_line)
    if inline_match:
        balance_units, nav, close_date, valuation = inline_match.groups()
        current_value = _parse_num(valuation)
        if current_value == 0:
            return None
        return {
            "date": pd.to_datetime(close_date, format="%d-%b-%Y"),
            "description": "Cur Value",
            "amt": -current_value,
            "units": 0.0,
            "nav": _parse_num(nav),
            "bal_units": _parse_num(balance_units),
        }

    bal_match = CLOSING_BAL_PATTERN.search(closing_line)
    balance_units = _parse_num(bal_match.group(1)) if bal_match else 0.0

    nav_match = None
    for idx in range(closing_idx - 1, max(state.folio_lines[folio_ord_num], closing_idx - 21), -1):
        nav_match = NAV_LINE_PATTERN.search(state.all_lines[idx])
        if nav_match:
            break
    if not nav_match:
        raise CasParseError(f"Could not parse closing balance for folio ordinal {folio_ord_num + 1}.")
    close_date, close_nav, valuation = nav_match.groups()
    current_value = _parse_num(valuation)
    if current_value == 0:
        return None
    return {
        "date": pd.to_datetime(close_date, format="%d-%b-%Y"),
        "description": "Cur Value",
        "amt": -current_value,
        "units": 0.0,
        "nav": _parse_num(close_nav),
        "bal_units": balance_units,
    }


def get_transactions(folio_ord_num: int, state: CasState) -> pd.DataFrame:
    validate_cas_state(state)
    working = state.all_lines[state.folio_lines[folio_ord_num] : state.closing_lines[folio_ord_num] + 1]
    rows = []
    for line in _transaction_blocks(working):
        parsed = _parse_transaction_line(line)
        if parsed is None:
            continue
        date_s, desc, amt_s, units_s, nav_s, bal_s = parsed
        rows.append(
            {
                "date": pd.to_datetime(date_s, format="%d-%b-%Y"),
                "description": desc.strip(),
                "amt": _parse_num(amt_s),
                "units": _parse_num(units_s),
                "nav": _parse_num(nav_s),
                "bal_units": _parse_num(bal_s),
            }
        )
    if not rows:
        raise CasParseError(f"No transaction rows parsed for folio ordinal {folio_ord_num + 1}.")

    txns = pd.DataFrame(rows)
    reinvest = txns[txns["description"].str.contains("IDCW Reinvest", case=False, na=False)].copy()
    if not reinvest.empty:
        reinvest["amt"] = -reinvest["amt"]
        txns = pd.concat([txns, reinvest], ignore_index=True)

    closing = _closing_row(folio_ord_num, state)
    if closing:
        txns = pd.concat([txns, pd.DataFrame([closing])], ignore_index=True)

    folio, pan = folio_and_pan(folio_ord_num, state)
    fund, advisor = fund_and_advisor(folio_ord_num, state)
    folio_idx = state.folio_lines[folio_ord_num]
    prior_amc = [idx for idx in state.amc_lines if idx < folio_idx]
    amc = state.all_lines[prior_amc[-1]] if prior_amc else ""
    txns = txns.sort_values("date").reset_index(drop=True)
    txns["days"] = (txns["date"].max() - txns["date"]).dt.days
    txns["years"] = txns["days"] / 365.25
    txns["amc"] = amc
    txns["fund"] = fund
    txns["advisor"] = advisor
    txns["folio"] = folio
    txns["pan"] = pan
    return txns[EMPTY_TXN_COLUMNS]


def _transaction_blocks(lines: Iterable[str]) -> list[str]:
    blocks: list[str] = []
    current: list[str] = []

    for raw_line in lines:
        line = raw_line.strip()
        if not line:
            continue

        starts_new_txn = bool(DATE_PREFIX_PATTERN.match(line))
        starts_section = bool(
            re.search(
                r"^(Folio No\s*:|Opening Unit Balance:|Closing Unit Balance:|[A-Z0-9]+\s*-|CAMSCASWS)",
                line,
                re.I,
            )
        )

        if starts_new_txn:
            if current:
                blocks.append(" ".join(current))
            current = [line]
        elif current and not starts_section:
            current.append(line)
        else:
            if current:
                blocks.append(" ".join(current))
                current = []
            blocks.append(line)

    if current:
        blocks.append(" ".join(current))
    return blocks

def _parse_transaction_line(line: str) -> tuple[str, str, str, str, str, str] | None:
    clean = line.strip()
    match = TRANSACTION_PATTERN.match(clean)
    if match:
        date_s, amt_s, nav_s, units_s, desc, bal_s = match.groups()
        return date_s, desc.strip(), amt_s, units_s, nav_s, bal_s

    match = R_TRANSACTION_PATTERN.match(clean)
    if match:
        date_s, desc, amt_s, units_s, nav_s, bal_s = match.groups()
        return date_s, desc.strip(), amt_s, units_s, nav_s, bal_s

    idcw_match = IDCW_PATTERN.match(clean)
    if idcw_match:
        date_s, desc, amt_s = idcw_match.groups()
        return date_s, desc.strip(), f"({amt_s})", "0", "0", "0"

    return None


def get_portfolio_transactions(state: CasState) -> pd.DataFrame:
    frames = [get_transactions(i, state) for i in range(len(state.folio_lines))]
    if not frames:
        return _empty_transactions()
    txns = pd.concat(frames, ignore_index=True).sort_values("date").reset_index(drop=True)
    txns["days"] = (txns["date"].max() - txns["date"]).dt.days
    txns["years"] = txns["days"] / 365.25
    return txns


def sample_pdf_paths(base_dir: str = "app/www/samples") -> list[str]:
    from pathlib import Path

    return [str(path) for path in sorted(Path(base_dir).glob("*.pdf"))]


def period_filter(dt_base: pd.DataFrame, start: date, end: date) -> pd.DataFrame:
    mask = (dt_base["description"] == "Cur Value") | (
        (dt_base["date"].dt.date >= start) & (dt_base["date"].dt.date <= end)
    )
    out = dt_base[mask].copy()
    if not out.empty:
        out["days"] = (out["date"].max() - out["date"]).dt.days
        out["years"] = out["days"] / 365.25
    return out
