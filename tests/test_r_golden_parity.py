from pathlib import Path

import pandas as pd
import pytest

from cas_streamlit.core.analytics import build_hierarchy_xirr_table, get_fund_summary_dt, get_mf_table_for_txns
from cas_streamlit.core.parser import get_portfolio_transactions, parse_cas_pdf, sample_pdf_paths


GOLDEN_ROOT = Path("tests/fixtures/r_golden")
MONEY_TOLERANCE = 0.01
# R's current XIRR implementation uses uniroot() with its default root tolerance.
# The resulting approximate root can drift by a few thousandths of a percentage
# point versus SciPy's converged root, even when the cash flows are identical.
XIRR_TOLERANCE = 0.01


def _sample_id(path: str) -> str:
    return Path(path).stem


def _read_golden(sample: str, name: str) -> pd.DataFrame:
    path = GOLDEN_ROOT / sample / f"{name}.csv"
    if not path.exists():
        pytest.fail(
            f"Missing R golden fixture: {path}. "
            "Regenerate with tools/generate_r_golden_fixtures.R."
        )
    return pd.read_csv(path)


def _normalise_dates(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    for col in out.columns:
        if "date" in col.lower():
            out[col] = pd.to_datetime(out[col], errors="coerce").dt.strftime("%Y-%m-%d")
    return out


def _normalise_transactions(df: pd.DataFrame) -> pd.DataFrame:
    cols = [
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
    out = _normalise_dates(df[cols])
    return out.sort_values(["folio", "date", "description", "amt"]).reset_index(drop=True)


def _python_outputs(sample_path: str) -> dict[str, pd.DataFrame]:
    txns = get_portfolio_transactions(parse_cas_pdf(sample_path, ""))
    fund_rows = [
        get_fund_summary_dt(txns, fund)
        for fund in txns.loc[txns["description"] != "Cur Value", "fund"].dropna().unique()
    ]
    fund_summary = pd.concat(fund_rows, ignore_index=True).rename(columns={"XIRR%": "XIRR"})
    folio_rows = [get_mf_table_for_txns(txns, folio) for folio in txns["folio"].dropna().unique()]
    folio_summary = pd.concat(folio_rows, ignore_index=True).rename(columns={"XIRR%": "XIRR"})
    enriched = txns.copy()
    enriched["AMC"] = enriched["amc"]
    enriched["Scheme"] = enriched["fund"]
    enriched["Folio"] = enriched["folio"]
    hierarchy = build_hierarchy_xirr_table(enriched, ["AMC", "Scheme", "Folio"])
    return {
        "transactions": _normalise_transactions(txns),
        "fund_summary": _normalise_dates(fund_summary.sort_values("Fund").reset_index(drop=True)),
        "folio_summary": _normalise_dates(folio_summary.sort_values("Folio").reset_index(drop=True)),
        "hierarchy_amc_scheme_folio": _normalise_dates(hierarchy.sort_values(["Path", "Level"]).reset_index(drop=True)),
    }


def _assert_frame_close(actual: pd.DataFrame, expected: pd.DataFrame, name: str) -> None:
    actual = actual.reset_index(drop=True)
    expected = expected.reset_index(drop=True)
    assert list(actual.columns) == list(expected.columns), name
    assert len(actual) == len(expected), name

    for col in expected.columns:
        if pd.api.types.is_numeric_dtype(expected[col]):
            tol = XIRR_TOLERANCE if "xirr" in col.lower() else MONEY_TOLERANCE
            pd.testing.assert_series_equal(
                pd.to_numeric(actual[col], errors="coerce"),
                pd.to_numeric(expected[col], errors="coerce"),
                check_names=False,
                check_dtype=False,
                atol=tol,
                rtol=0,
                obj=f"{name}.{col}",
            )
        else:
            left = actual[col].fillna("").astype(str)
            right = expected[col].fillna("").astype(str)
            pd.testing.assert_series_equal(left, right, check_names=False, obj=f"{name}.{col}")


@pytest.mark.parametrize("sample_path", sample_pdf_paths())
def test_python_outputs_match_r_golden_fixtures(sample_path):
    sample = _sample_id(sample_path)
    outputs = _python_outputs(sample_path)
    for name, actual in outputs.items():
        expected = _normalise_dates(_read_golden(sample, name))
        if name == "transactions":
            expected = _normalise_transactions(expected)
        _assert_frame_close(actual, expected, f"{sample}/{name}")
