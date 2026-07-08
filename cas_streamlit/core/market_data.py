from __future__ import annotations

import pickle
import re
import sqlite3
import time
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Iterable

import pandas as pd
import requests


APP_DIR = Path("app")
NAV_CACHE_DIR = APP_DIR / "nav_cache_py"
ISIN_DB_DIR = APP_DIR / "isin_db"
ISIN_DB_PATH = ISIN_DB_DIR / "isin.db"
ISIN_DB_URL = "https://casparser.atomcoder.com/isin.db"
ISIN_DB_MAX_DAYS = 30
SQLITE_MAGIC = b"SQLite format 3\x00"


def extract_fund_name(name: str) -> str:
    out = re.sub(r"^[A-Z0-9]+\s*-\s*", "", str(name))
    out = re.sub(r"\s*-?\s*ISIN:.*$", "", out, flags=re.I)
    out = re.sub(r"\(formerly[^)]*\)", "", out, flags=re.I)
    out = re.sub(r"\(Erstwhile[^)]*\)", "", out, flags=re.I)
    out = re.sub(r"\(Non.Demat\)", "", out, flags=re.I)
    out = re.sub(r"\(Advisor:.*", "", out, flags=re.I)
    out = re.sub(r"\s*Registrar\s*:.*", "", out, flags=re.I)
    return out.strip()


def normalize_fund_name(name: str) -> str:
    out = str(name).lower().replace("-", " ")
    out = re.sub(r"[^a-z0-9 ]", " ", out)
    out = re.sub(r"\s+", " ", out).strip()
    out = re.sub(r"\b([a-z]) ([a-z])\b", r"\1\2", out)
    out = re.sub(r"\b([a-z]) ([a-z])\b", r"\1\2", out)
    out = re.sub(r"([a-z])([0-9])", r"\1 \2", out)
    out = re.sub(r"([0-9])([a-z])", r"\1 \2", out)
    out = re.sub(r"\bfund of funds?\b", "fof", out)
    return re.sub(r"\s+", " ", out).strip()


def load_scheme_data(app_dir: Path = APP_DIR) -> tuple[pd.DataFrame, pd.DataFrame]:
    full_path = app_dir / "mf_codes.pkl"
    equity_path = app_dir / "mf_codes_equity.csv"
    all_schemes = pd.read_pickle(full_path) if full_path.exists() else pd.DataFrame(columns=["schemeName", "schemeCode"])
    equity = pd.read_csv(equity_path) if equity_path.exists() else pd.DataFrame(columns=["schemeName", "schemeCode"])
    return all_schemes, equity


def prepare_scheme_lookup(dt_mfs_all: pd.DataFrame) -> dict:
    norm_names = [normalize_fund_name(name) for name in dt_mfs_all.get("schemeName", [])]
    codes = [str(code) for code in dt_mfs_all.get("schemeCode", [])]
    return {
        "norm_names": norm_names,
        "codes": codes,
        "words": [list(dict.fromkeys(name.split())) for name in norm_names],
    }


def ensure_isin_db() -> bool:
    ISIN_DB_DIR.mkdir(parents=True, exist_ok=True)
    needs_download = not ISIN_DB_PATH.exists() or (
        datetime.now() - datetime.fromtimestamp(ISIN_DB_PATH.stat().st_mtime)
    ).days > ISIN_DB_MAX_DAYS
    if needs_download:
        try:
            resp = requests.get(
                ISIN_DB_URL,
                headers={"User-Agent": "casparser-isin 2025.3.1", "X-origin-casparser": "true"},
                timeout=60,
            )
            if resp.status_code == 200 and resp.content[:16] == SQLITE_MAGIC:
                ISIN_DB_PATH.write_bytes(resp.content)
        except Exception:
            pass
    return ISIN_DB_PATH.exists()


def isin_to_amfi(isin: str) -> str | None:
    if not isin or not str(isin).strip():
        return None
    if not ensure_isin_db():
        return None
    try:
        con = sqlite3.connect(str(ISIN_DB_PATH))
        row = con.execute("SELECT amfi_code FROM scheme WHERE isin = ? LIMIT 1", (isin.upper(),)).fetchone()
        con.close()
        return str(row[0]) if row and row[0] else None
    except Exception:
        return None


def match_fund_to_scheme(cas_fund_name: str, lookup: dict | Iterable[str], codes: Iterable[str] | None = None) -> str | None:
    if isinstance(lookup, dict):
        prepared = lookup
    else:
        norm_names = list(lookup)
        prepared = {
            "norm_names": norm_names,
            "codes": [str(code) for code in codes or []],
            "words": [list(dict.fromkeys(name.split())) for name in norm_names],
        }
    cleaned = extract_fund_name(cas_fund_name)
    norm_cas = normalize_fund_name(cleaned)
    cas_words = list(dict.fromkeys(norm_cas.split()))
    cas_word_set = set(cas_words)
    stopwords = {
        "direct",
        "regular",
        "plan",
        "growth",
        "idcw",
        "fund",
        "scheme",
        "option",
        "dividend",
        "bonus",
        "monthly",
        "quarterly",
        "annual",
        "reinvest",
        "payout",
        "weekly",
        "daily",
        "of",
        "the",
        "and",
        "fof",
    }

    def overlap_score(words: Iterable[str]) -> float:
        words = list(words)
        if not words:
            return 0.0
        inter = cas_word_set.intersection(words)
        if len(inter) < 4:
            return 0.0
        fwd = len(inter) / len(words)
        if fwd >= 0.8:
            return fwd
        rev = len(inter) / len(cas_words) if cas_words else 0.0
        return rev if rev >= 0.9 and any(word not in stopwords for word in inter) else 0.0

    isin_match = re.search(r"INF[A-Z0-9]{9}", str(cas_fund_name), re.I)
    if isin_match:
        amfi = isin_to_amfi(isin_match.group(0))
        if amfi and amfi in prepared["codes"]:
            idx = prepared["codes"].index(amfi)
            brand_words = set(prepared["words"][idx]).intersection(cas_word_set).difference(stopwords)
            if len(brand_words) >= 2:
                return amfi

    if norm_cas in prepared["norm_names"]:
        return prepared["codes"][prepared["norm_names"].index(norm_cas)]

    scores = [overlap_score(words) for words in prepared["words"]]
    if scores and max(scores) > 0:
        return prepared["codes"][scores.index(max(scores))]
    return None


def explain_fund_match(cas_fund_name: str, lookup: dict, scheme_code: str | None = None) -> pd.DataFrame:
    cleaned = extract_fund_name(cas_fund_name)
    norm_cas = normalize_fund_name(cleaned)
    method = "No match"
    confidence = None
    matched_name = None
    isin_match = re.search(r"INF[A-Z0-9]{9}", str(cas_fund_name), re.I)
    isin = isin_match.group(0).upper() if isin_match else None
    if scheme_code and str(scheme_code) in lookup["codes"]:
        idx = lookup["codes"].index(str(scheme_code))
        matched_name = lookup["norm_names"][idx]
        if matched_name == norm_cas:
            method, confidence = "Exact normalized name", 1.0
        elif isin and isin_to_amfi(isin) == str(scheme_code):
            method, confidence = "ISIN", 1.0
        else:
            cas_words = set(norm_cas.split())
            matched_words = set(matched_name.split())
            score = max(
                len(cas_words & matched_words) / max(len(matched_words), 1),
                len(cas_words & matched_words) / max(len(cas_words), 1),
            )
            method = "High-confidence word overlap" if score >= 0.9 else "Approximate word overlap"
            confidence = round(score, 3)
    return pd.DataFrame(
        [
            {
                "Fund": cleaned,
                "SchemeCode": scheme_code,
                "Method": method,
                "Confidence": confidence,
                "ISIN": isin,
                "MatchedName": matched_name,
            }
        ]
    )


def explain_fund_matches(fund_scheme_map: dict[str, str | None], lookup: dict) -> pd.DataFrame:
    frames = [explain_fund_match(name, lookup, code) for name, code in fund_scheme_map.items()]
    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()


def get_navs(scheme_code: str) -> pd.DataFrame:
    last_error: Exception | None = None
    for attempt in range(3):
        try:
            data = requests.get(f"https://api.mfapi.in/mf/{scheme_code}", timeout=15).json()
            rows = pd.DataFrame(data["data"])
            rows["date"] = pd.to_datetime(rows["date"], format="%d-%m-%Y")
            rows["nav"] = rows["nav"].astype(float)
            rows = rows.sort_values("date")
            all_dates = pd.date_range(rows["date"].min(), rows["date"].max(), freq="D")
            out = pd.DataFrame({"date": all_dates}).merge(rows, on="date", how="left")
            out["nav"] = out["nav"].ffill()
            return out[out["nav"].notna() & (out["nav"] != 0)].reset_index(drop=True)
        except Exception as exc:
            last_error = exc
            if attempt < 2:
                time.sleep(2**attempt)
    raise RuntimeError(f"NAV fetch failed for {scheme_code}: {last_error}")


def _cache_file(scheme_code: str) -> Path:
    safe_code = re.sub(r"[^A-Za-z0-9_-]", "_", str(scheme_code))
    return NAV_CACHE_DIR / f"{safe_code}.pkl"


def get_cached_navs(scheme_code: str, required_date: date | pd.Timestamp | None = None) -> pd.DataFrame | None:
    NAV_CACHE_DIR.mkdir(parents=True, exist_ok=True)
    if required_date is None:
        required_date = date.today()
    required = pd.Timestamp(required_date).date()
    path = _cache_file(scheme_code)
    if path.exists():
        try:
            cached = pickle.loads(path.read_bytes())
            max_date = pd.Timestamp(cached["date"].max()).date()
            if max_date >= required - timedelta(days=7):
                return cached
        except Exception:
            pass
    try:
        navs = get_navs(str(scheme_code))
        path.write_bytes(pickle.dumps(navs))
        return navs
    except Exception:
        if path.exists():
            try:
                return pickle.loads(path.read_bytes())
            except Exception:
                return None
    return None


def pre_warm_navs(
    fund_scheme_map: dict[str, str | None],
    required_date: date | None = None,
    progress_fn=None,
) -> pd.DataFrame:
    NAV_CACHE_DIR.mkdir(parents=True, exist_ok=True)
    required = required_date or date.today()
    rows = []
    for fund, scheme_code in fund_scheme_map.items():
        if progress_fn:
            progress_fn(fund)
        if scheme_code is None or pd.isna(scheme_code):
            rows.append({"Fund": extract_fund_name(fund), "SchemeCode": None, "Source": "No match", "NAVsUpTo": None})
            continue
        source = "API - new"
        path = _cache_file(str(scheme_code))
        if path.exists():
            try:
                cached = pickle.loads(path.read_bytes())
                max_date = pd.Timestamp(cached["date"].max()).date()
                if max_date >= required - timedelta(days=7):
                    rows.append(
                        {
                            "Fund": extract_fund_name(fund),
                            "SchemeCode": str(scheme_code),
                            "Source": "Cache",
                            "NAVsUpTo": str(max_date),
                        }
                    )
                    continue
                source = "API - refreshed"
            except Exception:
                source = "API - new"
        navs = get_cached_navs(str(scheme_code), required)
        if navs is None or navs.empty:
            rows.append(
                {
                    "Fund": extract_fund_name(fund),
                    "SchemeCode": str(scheme_code),
                    "Source": "Fetch failed",
                    "NAVsUpTo": None,
                }
            )
        else:
            rows.append(
                {
                    "Fund": extract_fund_name(fund),
                    "SchemeCode": str(scheme_code),
                    "Source": source if source.startswith("API") else "Cache",
                    "NAVsUpTo": str(pd.Timestamp(navs["date"].max()).date()),
                }
            )
    return pd.DataFrame(rows, columns=["Fund", "SchemeCode", "Source", "NAVsUpTo"])


def nav_on_or_before(dt_navs: pd.DataFrame, dates: Iterable[date | pd.Timestamp]) -> pd.DataFrame:
    target_dates = pd.to_datetime(list(dates)).astype("datetime64[ns]")
    if len(target_dates) == 0:
        return pd.DataFrame(columns=["date", "nav_date", "nav"])
    if dt_navs is None or dt_navs.empty:
        return pd.DataFrame({"date": target_dates, "nav_date": pd.NaT, "nav": pd.NA})
    navs = dt_navs[["date", "nav"]].copy().sort_values("date")
    navs["date"] = pd.to_datetime(navs["date"]).astype("datetime64[ns]")
    targets = pd.DataFrame({"date": target_dates}).sort_values("date")
    joined = pd.merge_asof(targets, navs.rename(columns={"date": "nav_date"}), left_on="date", right_on="nav_date")
    return joined[["date", "nav_date", "nav"]]


def build_fund_category_map(
    funds: Iterable[str],
    fund_scheme_map: dict[str, str | None],
    dt_navall: pd.DataFrame | None = None,
) -> pd.DataFrame:
    if dt_navall is None or dt_navall.empty:
        return pd.DataFrame(
            [{"Fund": fund, "SchemeType": None, "Category": None, "SubCategory": None} for fund in funds]
        )
    rows = []
    code_lookup = dt_navall.drop_duplicates("SchemeCode").set_index(dt_navall["SchemeCode"].astype(str))
    for fund in funds:
        row = {"Fund": fund, "SchemeType": None, "Category": None, "SubCategory": None}
        code = fund_scheme_map.get(fund)
        if code is not None and str(code) in code_lookup.index:
            found = code_lookup.loc[str(code)]
            row.update(
                {
                    "SchemeType": found.get("SchemeType"),
                    "Category": found.get("Category"),
                    "SubCategory": found.get("SubCategory"),
                }
            )
        rows.append(row)
    return pd.DataFrame(rows)
