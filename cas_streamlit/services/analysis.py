from __future__ import annotations

import math
from dataclasses import dataclass
from datetime import date
from typing import BinaryIO

import numpy as np
import pandas as pd

from cas_streamlit.core.analytics import (
    build_hierarchy_xirr_table,
    build_quality_diagnostics,
    get_fund_summary_dt,
    get_mf_table_for_txns,
    recalc_xirr_basis,
    xirr,
)
from cas_streamlit.core.market_data import (
    build_fund_category_map,
    explain_fund_matches,
    get_cached_navs,
    load_scheme_data,
    match_fund_to_scheme,
    nav_on_or_before,
    pre_warm_navs,
    prepare_scheme_lookup,
)
from cas_streamlit.core.parser import CasParseError, parse_cas_pdf, period_filter, get_portfolio_transactions


@dataclass
class AnalysisResult:
    dt_base_txns: pd.DataFrame
    dt_folio_xirrs: pd.DataFrame
    fund_scheme_map: dict[str, str | None]
    fund_match_log: pd.DataFrame
    fund_category_map: pd.DataFrame
    nav_status_log: pd.DataFrame
    portfolio_curve: pd.DataFrame
    xirr_curve: pd.DataFrame
    diagnostics: pd.DataFrame
    equity_schemes: pd.DataFrame


def run_analysis(file: str | BinaryIO, password: str = "", progress_fn=None) -> AnalysisResult:
    state = parse_cas_pdf(file, password)
    dt_base = get_portfolio_transactions(state)
    if dt_base.empty:
        raise CasParseError("No transactions could be parsed from this PDF.")

    all_schemes, equity_schemes = load_scheme_data()
    lookup = prepare_scheme_lookup(all_schemes)
    funds = sorted(dt_base.loc[dt_base["description"] != "Cur Value", "fund"].dropna().unique())
    fund_scheme_map = {fund: match_fund_to_scheme(fund, lookup) for fund in funds}
    fund_match_log = explain_fund_matches(fund_scheme_map, lookup)

    folio_rows = [get_mf_table_for_txns(dt_base, folio) for folio in dt_base["folio"].dropna().unique()]
    dt_folio = pd.concat(folio_rows, ignore_index=True) if folio_rows else pd.DataFrame()
    nav_log = pre_warm_navs(fund_scheme_map, required_date=date.today(), progress_fn=progress_fn)
    category_map = build_fund_category_map(funds, fund_scheme_map)
    curve, _warnings = get_portfolio_curve(dt_base, fund_scheme_map)
    xirr_curve = compute_xirr_curve(dt_base, curve)
    diagnostics = build_quality_diagnostics(dt_base, fund_scheme_map, category_map, nav_log)
    return AnalysisResult(
        dt_base_txns=dt_base,
        dt_folio_xirrs=dt_folio,
        fund_scheme_map=fund_scheme_map,
        fund_match_log=fund_match_log,
        fund_category_map=category_map,
        nav_status_log=nav_log,
        portfolio_curve=curve,
        xirr_curve=xirr_curve,
        diagnostics=diagnostics,
        equity_schemes=equity_schemes,
    )


def portfolio_value_at(
    dt_base: pd.DataFrame,
    target_date: date | pd.Timestamp,
    fund_scheme_map: dict[str, str | None],
    inclusive: bool = False,
) -> dict:
    target = pd.Timestamp(target_date)
    total = 0.0
    warnings: list[str] = []
    date_mask = dt_base["date"].le(target) if inclusive else dt_base["date"].lt(target)
    for fund in dt_base.loc[dt_base["description"] != "Cur Value", "fund"].dropna().unique():
        prior = dt_base[(dt_base["fund"] == fund) & date_mask & (dt_base["description"] != "Cur Value")]
        if prior.empty:
            continue
        units = prior.sort_values("date").groupby("folio", dropna=False)["bal_units"].last().sum()
        if units <= 0:
            continue
        scheme_code = fund_scheme_map.get(fund)
        if scheme_code is None or pd.isna(scheme_code):
            warnings.append(f"No scheme match for fund: {fund}")
            continue
        navs = get_cached_navs(str(scheme_code), target.date())
        nav = nav_on_or_before(navs, [target]).iloc[0] if navs is not None else None
        if nav is None or pd.isna(nav["nav"]):
            warnings.append(f"No NAV available for {fund} on or before {target.date()}")
            continue
        total += float(units) * float(nav["nav"])
    return {"value": total, "warnings": warnings}


def get_portfolio_curve(
    dt_base: pd.DataFrame,
    fund_scheme_map: dict[str, str | None],
    sample_by: str = "MS",
) -> tuple[pd.DataFrame, list[str]]:
    start = dt_base.loc[dt_base["description"] != "Cur Value", "date"].min()
    end = dt_base.loc[dt_base["description"] == "Cur Value", "date"].max()
    sample_dates = pd.date_range(start, end, freq=sample_by)
    if end not in sample_dates:
        sample_dates = sample_dates.append(pd.DatetimeIndex([end]))
    portfolio_values = np.zeros(len(sample_dates))
    failed: list[str] = []
    samples = pd.DataFrame({"date": sample_dates})

    for fund in dt_base.loc[dt_base["description"] != "Cur Value", "fund"].dropna().unique():
        scheme_code = fund_scheme_map.get(fund)
        if scheme_code is None or pd.isna(scheme_code):
            failed.append(fund)
            continue
        navs = get_cached_navs(str(scheme_code), end.date())
        if navs is None or navs.empty:
            failed.append(fund)
            continue
        navs = navs.sort_values("date")
        fund_txns = dt_base[(dt_base["fund"] == fund) & (dt_base["description"] != "Cur Value")].sort_values("date")
        units_vec = np.zeros(len(samples))
        for _folio, rows in fund_txns.groupby("folio", dropna=False):
            balances = rows.groupby("date", as_index=False)["bal_units"].last().sort_values("date")
            joined = pd.merge_asof(samples, balances, on="date", direction="backward")
            units_vec += joined["bal_units"].fillna(0).to_numpy()
        nav_joined = pd.merge_asof(samples, navs[["date", "nav"]], on="date", direction="backward")
        portfolio_values += units_vec * nav_joined["nav"].fillna(0).to_numpy()

    daily_net = (
        dt_base[dt_base["description"] != "Cur Value"]
        .groupby("date", as_index=False)["amt"]
        .sum()
        .sort_values("date")
    )
    daily_net["cum_amt"] = daily_net["amt"].cumsum()
    cum_joined = pd.merge_asof(samples, daily_net[["date", "cum_amt"]], on="date", direction="backward")
    invested = cum_joined["cum_amt"].fillna(0).clip(lower=0).to_numpy()
    return (
        pd.DataFrame(
            {
                "date": sample_dates,
                "portfolio_value": np.round(portfolio_values, 2),
                "net_invested": np.round(invested, 2),
                "gains": np.round(portfolio_values - invested, 2),
            }
        ),
        failed,
    )


def compute_xirr_curve(dt_base: pd.DataFrame, curve: pd.DataFrame) -> pd.DataFrame:
    cash = dt_base[
        (dt_base["description"] != "Cur Value")
        & (~dt_base["description"].str.match(r"^Switch", case=False, na=False))
    ][["date", "amt"]]
    values = []
    for _, row in curve.iterrows():
        as_of = row["date"]
        portfolio_value = row["portfolio_value"]
        prior = cash[cash["date"] <= as_of]
        if prior.empty or portfolio_value <= 0:
            values.append(np.nan)
            continue
        flows = pd.concat(
            [prior, pd.DataFrame([{"date": as_of, "amt": -portfolio_value}])],
            ignore_index=True,
        )
        val = xirr(recalc_xirr_basis(flows, as_of=as_of))
        values.append(val * 100 if not math.isnan(val) else np.nan)
    return pd.DataFrame({"date": curve["date"], "xirr": values})


def compute_period_summary(
    result: AnalysisResult,
    start: date,
    end: date,
) -> tuple[pd.DataFrame, float | None, list[str]]:
    dt_base = result.dt_base_txns
    first_txn = dt_base.loc[dt_base["description"] != "Cur Value", "date"].min().date()
    cas_close = dt_base.loc[dt_base["description"] == "Cur Value", "date"].max().date()
    period = period_filter(dt_base, start, end)
    cash = period[period["description"] != "Cur Value"]
    investment = cash.loc[cash["amt"] > 0, "amt"].sum()
    redemption = -cash.loc[cash["amt"] < 0, "amt"].sum()
    warnings: list[str] = []

    if start <= first_txn:
        start_value = 0.0
    else:
        start_result = portfolio_value_at(dt_base, start, result.fund_scheme_map)
        start_value = start_result["value"]
        warnings.extend(start_result["warnings"])

    if end >= cas_close:
        end_value = -dt_base.loc[dt_base["description"] == "Cur Value", "amt"].sum()
    else:
        end_result = portfolio_value_at(dt_base, end, result.fund_scheme_map, inclusive=True)
        end_value = end_result["value"]
        warnings.extend(end_result["warnings"])

    table = pd.DataFrame(
        {
            "Metric": [
                "Start Value",
                "Investment during period",
                "Redemption during period",
                "Net Investment",
                "End Value",
                "Total Gains",
            ],
            "Amount": [
                start_value,
                investment,
                redemption,
                investment - redemption,
                end_value,
                end_value - start_value - (investment - redemption),
            ],
        }
    )
    period_flows = cash[~cash["description"].str.match(r"^Switch", case=False, na=False)][["date", "amt"]].copy()
    flow_rows = []
    if start_value > 0:
        flow_rows.append(pd.DataFrame([{"date": pd.Timestamp(start), "amt": start_value}]))
    if not period_flows.empty:
        flow_rows.append(period_flows)
    flow_rows.append(pd.DataFrame([{"date": pd.Timestamp(end), "amt": -end_value}]))
    flows = pd.concat(flow_rows, ignore_index=True)
    period_xirr = xirr(recalc_xirr_basis(flows, as_of=pd.Timestamp(end)))
    return table, (period_xirr if not math.isnan(period_xirr) else None), warnings


def fund_summary(result: AnalysisResult, start: date, end: date) -> pd.DataFrame:
    # Fund Summary intentionally uses the full base CAS, matching the R fix that
    # kept this master summary independent from the selected analysis period.
    rows = [
        get_fund_summary_dt(result.dt_base_txns, fund)
        for fund in result.dt_base_txns.loc[result.dt_base_txns["description"] != "Cur Value", "fund"].dropna().unique()
    ]
    return pd.concat(rows, ignore_index=True) if rows else pd.DataFrame()


def filtered_transactions(result: AnalysisResult, start: date, end: date) -> pd.DataFrame:
    return period_filter(result.dt_base_txns, start, end)


def hierarchy_table(result: AnalysisResult, group_cols: list[str]) -> pd.DataFrame:
    dt = result.dt_base_txns.copy()
    dt["AMC"] = dt["amc"]
    dt["Scheme"] = dt["fund"]
    dt["Folio"] = dt["folio"]
    if "Category" not in dt.columns:
        cats = result.fund_category_map.rename(columns={"Fund": "fund"})
        dt = dt.merge(cats[["fund", "Category", "SubCategory"]], on="fund", how="left")
    return build_hierarchy_xirr_table(dt, group_cols)
