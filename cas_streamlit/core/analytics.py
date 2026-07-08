from __future__ import annotations

import math
from datetime import date
from typing import Iterable

import numpy as np
import pandas as pd
from scipy.optimize import brentq


def external_cashflows(dt_txns: pd.DataFrame, exclude_switches: bool = True) -> pd.DataFrame:
    out = dt_txns[dt_txns["description"] != "Cur Value"].copy()
    if exclude_switches:
        out = out[~out["description"].str.match(r"^Switch", case=False, na=False)].copy()
    return out


def position_cashflows(dt_txns: pd.DataFrame) -> pd.DataFrame:
    return dt_txns[dt_txns["description"] != "Cur Value"].copy()


def recalc_xirr_basis(dt_txns: pd.DataFrame, as_of: pd.Timestamp | date | None = None) -> pd.DataFrame:
    if dt_txns.empty:
        return dt_txns.copy()
    out = dt_txns.copy()
    if as_of is None:
        as_of = out["date"].max()
    as_of_ts = pd.Timestamp(as_of)
    out["date"] = pd.to_datetime(out["date"])
    out["days"] = (as_of_ts - out["date"]).dt.days
    out["years"] = out["days"] / 365.25
    return out


def xirr(dt_txns: pd.DataFrame) -> float:
    if dt_txns.empty or not (dt_txns["amt"].gt(0).any() and dt_txns["amt"].lt(0).any()):
        return float("nan")

    def fvs(rate: float) -> float:
        return float((dt_txns["amt"] * (1 + rate) ** dt_txns["years"]).sum())

    for upper in (1.0, 10.0, 100.0):
        try:
            low_v, high_v = fvs(-0.9999), fvs(upper)
            if math.isnan(low_v) or math.isnan(high_v) or low_v * high_v > 0:
                continue
            return float(brentq(fvs, -0.9999, upper, xtol=1e-4))
        except Exception:
            continue
    return float("nan")


def get_mf_summary(
    dt_txns: pd.DataFrame,
    folio_ord_num: int = -1,
    folio_id: str = "",
    include_switches: bool = True,
) -> pd.DataFrame:
    if dt_txns.empty:
        return pd.DataFrame()
    dt = dt_txns.copy()
    current_value = -dt.loc[dt["description"] == "Cur Value", "amt"].sum()
    flows = external_cashflows(dt, exclude_switches=not include_switches)[["date", "amt"]]
    closing = dt.loc[dt["description"] == "Cur Value", ["date", "amt"]]
    xirr_flows = recalc_xirr_basis(pd.concat([flows, closing], ignore_index=True))
    xirr_val = xirr(xirr_flows)

    cash_txns = position_cashflows(dt) if include_switches else external_cashflows(dt)
    cash_in = cash_txns.loc[cash_txns["amt"] > 0, "amt"].sum()
    cash_out = -cash_txns.loc[cash_txns["amt"] < 0, "amt"].sum()
    redemptions = cash_out
    total_out = redemptions + current_value
    cost_of_redemptions = cash_in * redemptions / total_out if total_out > 0 else 0.0
    realized = redemptions - cost_of_redemptions
    unrealized = current_value - (cash_in - cost_of_redemptions)

    row = {
        "Cur.Value": current_value,
        "Invested": cash_in,
        "Redeemed": redemptions,
        "RealizedGains": realized,
        "UnrealizedGains": unrealized,
        "XIRR": xirr_val * 100 if not math.isnan(xirr_val) else np.nan,
        "StartDate": dt["date"].min(),
        "RecentDate": dt["date"].max(),
    }
    if folio_ord_num == -1:
        row = {"Folio": folio_id, **row}
    else:
        row = {"Fund": dt["fund"].iloc[0] if "fund" in dt else "", **row}
    return pd.DataFrame([row])


def get_mf_table_for_txns(dt_all_txns: pd.DataFrame, folio_id: str) -> pd.DataFrame:
    dt = dt_all_txns[dt_all_txns["folio"] == folio_id].copy()
    summary = get_mf_summary(recalc_xirr_basis(dt), -1, folio_id)
    return summary.rename(columns={"XIRR": "XIRR%"})


def get_fund_summary_dt(dt_all: pd.DataFrame, fund_name: str) -> pd.DataFrame:
    dt = dt_all[dt_all["fund"] == fund_name].copy()
    if dt.empty:
        return pd.DataFrame()
    return get_mf_summary(recalc_xirr_basis(dt), 1).rename(columns={"XIRR": "XIRR%"})


def summarise_position_group(dt_group: pd.DataFrame) -> dict:
    txns = position_cashflows(dt_group)
    cur_val = dt_group[dt_group["description"] == "Cur Value"]
    invested = txns.loc[txns["amt"] > 0, "amt"].sum()
    redeemed = -txns.loc[txns["amt"] < 0, "amt"].sum()
    current_value = -cur_val["amt"].sum()
    gains = current_value - invested + redeemed
    flows = pd.concat([txns[["date", "amt"]], cur_val[["date", "amt"]]], ignore_index=True)
    xirr_val = xirr(recalc_xirr_basis(flows)) if not flows.empty else float("nan")
    return {
        "Cur Value": round(current_value, 2),
        "Invested": round(invested, 2),
        "Redeemed": round(redeemed, 2),
        "Net Invested": round(invested - redeemed, 2),
        "Gains": round(gains, 2),
        "XIRR%": round(xirr_val * 100, 3) if not math.isnan(xirr_val) else np.nan,
    }


def build_hierarchy_xirr_table(dt_enriched: pd.DataFrame, group_cols: Iterable[str]) -> pd.DataFrame:
    cols = list(group_cols)
    if dt_enriched.empty or not cols:
        return pd.DataFrame()
    rows: list[dict] = []
    for depth in range(1, len(cols) + 1):
        level_cols = cols[:depth]
        grouped = dt_enriched.groupby(level_cols, dropna=False, sort=True)
        for keys, group in grouped:
            if not isinstance(keys, tuple):
                keys = (keys,)
            values = {col: pd.NA for col in cols}
            values.update(dict(zip(level_cols, keys)))
            summary = summarise_position_group(group)
            rows.append(
                {
                    "Level": depth,
                    "Group": f"{'-- ' * (depth - 1)}{keys[-1]}",
                    "Path": " / ".join(str(k) for k in keys),
                    **values,
                    **summary,
                }
            )
    return pd.DataFrame(rows)


def analytics_group_label_to_col(labels: Iterable[str]) -> list[str]:
    return ["SubCategory" if label == "Sub-Category" else label for label in labels]


def sanitize_custom_analytics_groups(labels: Iterable[str] | None) -> list[str]:
    if not labels:
        return []
    valid = ["AMC", "Category", "Sub-Category", "Scheme", "Folio"]
    seen: list[str] = []
    for label in labels:
        if label in valid and label not in seen:
            seen.append(label)
    if "Scheme" not in seen:
        return seen
    scheme_idx = seen.index("Scheme")
    after = [label for label in seen[scheme_idx + 1 :] if label == "Folio"]
    return seen[: scheme_idx + 1] + after


def available_custom_analytics_groups(labels: Iterable[str] | None) -> list[str]:
    valid = ["AMC", "Category", "Sub-Category", "Scheme", "Folio"]
    sanitized = sanitize_custom_analytics_groups(labels)
    if "Scheme" not in sanitized:
        return valid
    return list(dict.fromkeys(sanitized + ["Folio"]))


def get_performance_contributors(dt_leaves: pd.DataFrame, top_n: int = 10) -> dict[str, pd.DataFrame]:
    if dt_leaves.empty:
        return {"top": pd.DataFrame(), "bottom": pd.DataFrame()}
    dt_leaves = ensure_gains_column(dt_leaves)
    grouped = (
        dt_leaves.groupby("Scheme", dropna=False)[["Cur Value", "Invested", "Redeemed", "Gains"]]
        .sum()
        .reset_index()
    )
    total_abs_gains = grouped["Gains"].abs().sum()
    grouped["Gain Share"] = grouped["Gains"] / total_abs_gains * 100 if total_abs_gains > 0 else np.nan
    return {
        "top": grouped.sort_values("Gains", ascending=False).head(top_n).reset_index(drop=True),
        "bottom": grouped.sort_values("Gains", ascending=True).head(top_n).reset_index(drop=True),
    }


def ensure_gains_column(dt_leaves: pd.DataFrame) -> pd.DataFrame:
    out = dt_leaves.copy()
    if "Gains" not in out.columns:
        if {"RealizedGains", "UnrealizedGains"}.issubset(out.columns):
            out["Gains"] = out["RealizedGains"].fillna(0) + out["UnrealizedGains"].fillna(0)
        elif {"Cur Value", "Invested", "Redeemed"}.issubset(out.columns):
            out["Gains"] = out["Cur Value"].fillna(0) - out["Invested"].fillna(0) + out["Redeemed"].fillna(0)
        else:
            out["Gains"] = np.nan
    return out


def get_allocation_summary(dt_leaves: pd.DataFrame, group_col: str, top_n: int | None = None) -> pd.DataFrame:
    if dt_leaves.empty:
        return pd.DataFrame()
    dt_leaves = ensure_gains_column(dt_leaves)
    out = dt_leaves.groupby(group_col, dropna=False)[["Cur Value", "Gains"]].sum().reset_index()
    total_value = out["Cur Value"].sum()
    out["Weight"] = out["Cur Value"] / total_value * 100 if total_value > 0 else np.nan
    out = out.sort_values("Cur Value", ascending=False).rename(columns={group_col: "Group"})
    return out.head(top_n) if top_n else out


def build_quality_diagnostics(
    dt_base: pd.DataFrame,
    fund_scheme_map: dict[str, str | None],
    fund_category_map: pd.DataFrame,
    nav_status_log: pd.DataFrame,
    period_warnings: Iterable[str] = (),
) -> pd.DataFrame:
    funds = dt_base.loc[dt_base["description"] != "Cur Value", "fund"].dropna().unique()
    unmatched = [name for name, code in fund_scheme_map.items() if code is None or pd.isna(code)]
    unknown_categories = pd.DataFrame()
    if not fund_category_map.empty:
        unknown_categories = fund_category_map[
            fund_category_map["Category"].isna()
            | (fund_category_map["Category"] == "(Unknown)")
            | fund_category_map["SubCategory"].isna()
            | (fund_category_map["SubCategory"] == "(Unknown)")
        ]
    switch_rows = dt_base[dt_base["description"].str.match(r"^Switch", case=False, na=False)]
    stale_nav = pd.DataFrame()
    if nav_status_log is not None and not nav_status_log.empty:
        stale_nav = nav_status_log[nav_status_log["Source"].str.contains("stale|failed|No match", case=False, na=False)]
    warnings = list(period_warnings)
    return pd.DataFrame(
        {
            "Check": [
                "Funds parsed",
                "Unmatched NAV schemes",
                "Unknown categories",
                "Switch transactions",
                "NAV cache issues",
                "Period warnings",
            ],
            "Count": [
                len(funds),
                len(unmatched),
                unknown_categories["Fund"].nunique() if "Fund" in unknown_categories else 0,
                len(switch_rows),
                len(stale_nav),
                len(warnings),
            ],
            "Severity": [
                "Info",
                "High" if unmatched else "OK",
                "Medium" if not unknown_categories.empty else "OK",
                "Info" if not switch_rows.empty else "OK",
                "Medium" if not stale_nav.empty else "OK",
                "Medium" if warnings else "OK",
            ],
            "Detail": [
                "Distinct fund strings found in the CAS.",
                "Funds without a resolved mfapi/AMFI scheme code.",
                "Funds missing AMFI category or sub-category metadata.",
                "Internal switch rows detected and excluded from portfolio-level cash flows.",
                "NAV rows that used stale cache, failed fetches, or had no scheme match.",
                "Warnings generated by selected-period valuation.",
            ],
        }
    )
