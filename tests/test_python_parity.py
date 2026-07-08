import math
from datetime import date

import pandas as pd

from cas_streamlit.core.analytics import (
    analytics_group_label_to_col,
    available_custom_analytics_groups,
    build_hierarchy_xirr_table,
    build_quality_diagnostics,
    ensure_gains_column,
    get_mf_summary,
    get_performance_contributors,
    get_allocation_summary,
    recalc_xirr_basis,
    sanitize_custom_analytics_groups,
    external_cashflows,
    xirr,
)
from cas_streamlit.core.market_data import nav_on_or_before, prepare_scheme_lookup, match_fund_to_scheme
from cas_streamlit.services.analysis import fund_summary, compute_period_summary, AnalysisResult


def test_xirr_handles_normal_annual_return():
    dt = recalc_xirr_basis(pd.DataFrame({"date": pd.to_datetime(["2024-01-01", "2025-01-01"]), "amt": [100, -110]}))
    assert round(xirr(dt), 3) == 0.1


def test_fund_summaries_include_switch_in_cash_flows_for_position_returns():
    dt = pd.DataFrame(
        {
            "date": pd.to_datetime(["2018-06-15", "2020-12-10", "2026-07-03"]),
            "description": ["Switch In (Decimal Change)", "Purchase (Continuous Offer)", "Cur Value"],
            "amt": [300192.390, 149992.500, -683094.010],
            "fund": ["Kotak Liquid Fund"] * 3,
        }
    )
    summary = get_mf_summary(recalc_xirr_basis(dt), folio_ord_num=1)
    assert round(summary["Invested"].iloc[0], 2) == 450184.89
    assert round(summary["XIRR"].iloc[0], 3) == 5.911


def test_external_cash_flows_exclude_switches_for_portfolio_returns():
    dt = pd.DataFrame({"description": ["Switch In", "Purchase", "Switch Out", "Cur Value"], "amt": [100, 50, -80, -90]})
    assert external_cashflows(dt)["amt"].tolist() == [50]


def test_analytics_hierarchy_recalculates_xirr_at_each_level():
    dt = pd.DataFrame(
        {
            "AMC": ["Kotak Mutual Fund"] * 3,
            "Category": ["Debt Scheme"] * 3,
            "SubCategory": ["Liquid Fund"] * 3,
            "Scheme": ["Kotak Liquid Fund Direct Plan Growth"] * 3,
            "folio": ["F1"] * 3,
            "date": pd.to_datetime(["2018-06-15", "2020-12-10", "2026-07-03"]),
            "description": ["Switch In (Decimal Change)", "Purchase (Continuous Offer)", "Cur Value"],
            "amt": [300192.390, 149992.500, -683094.010],
        }
    )
    hierarchy = build_hierarchy_xirr_table(dt, ["AMC", "Category", "SubCategory", "Scheme"])
    assert sorted(hierarchy["Level"].unique().tolist()) == [1, 2, 3, 4]
    assert hierarchy[hierarchy["Level"] == 1]["Cur Value"].iloc[0] == 683094.01
    assert hierarchy[hierarchy["Level"] == 1]["Invested"].iloc[0] == 450184.89
    assert hierarchy[hierarchy["Level"] == 4]["XIRR%"].iloc[0] == 5.911


def test_custom_analytics_grouping_only_allows_folio_below_scheme():
    assert sanitize_custom_analytics_groups(["Category", "Scheme", "AMC", "Folio"]) == ["Category", "Scheme", "Folio"]
    assert sanitize_custom_analytics_groups(["AMC", "Folio", "Category", "Scheme"]) == ["AMC", "Folio", "Category", "Scheme"]
    assert available_custom_analytics_groups(["Category", "Scheme"]) == ["Category", "Scheme", "Folio"]
    assert analytics_group_label_to_col(["Sub-Category", "Scheme", "Folio"]) == ["SubCategory", "Scheme", "Folio"]


def test_nav_on_or_before_uses_previous_available_date():
    navs = pd.DataFrame({"date": pd.to_datetime(["2024-01-01", "2024-01-03"]), "nav": [10, 12]})
    out = nav_on_or_before(navs, [date(2024, 1, 2), date(2024, 1, 3)])
    assert out["nav"].tolist() == [10, 12]
    assert out["nav_date"].dt.date.tolist() == [date(2024, 1, 1), date(2024, 1, 3)]


def test_scheme_lookup_matches_exact_normalized_names():
    lookup = prepare_scheme_lookup(pd.DataFrame({"schemeName": ["Alpha Growth Fund Direct Plan Growth"], "schemeCode": [123]}))
    assert match_fund_to_scheme("Alpha Growth Fund Direct Plan Growth", lookup) == "123"


def test_performance_contributors_include_gain_share_percentages():
    leaves = pd.DataFrame(
        {
            "Scheme": ["Fund A", "Fund B", "Fund C"],
            "Cur Value": [120, 90, 80],
            "Invested": [100, 100, 100],
            "Redeemed": [0, 0, 0],
            "Gains": [20, -10, -20],
        }
    )
    contributors = get_performance_contributors(leaves, top_n=3)
    assert contributors["top"].loc[contributors["top"]["Scheme"] == "Fund A", "Gain Share"].iloc[0] == 40
    assert contributors["bottom"].loc[contributors["bottom"]["Scheme"] == "Fund C", "Gain Share"].iloc[0] == -40


def test_insights_helpers_derive_gains_from_fund_summary_columns():
    leaves = pd.DataFrame(
        {
            "Scheme": ["Fund A", "Fund B"],
            "Cur Value": [120, 90],
            "Invested": [100, 100],
            "Redeemed": [0, 0],
            "RealizedGains": [5, -2],
            "UnrealizedGains": [15, -8],
        }
    )

    with_gains = ensure_gains_column(leaves)
    assert with_gains["Gains"].tolist() == [20, -10]
    allocation = get_allocation_summary(leaves, "Scheme")
    contributors = get_performance_contributors(leaves)
    assert allocation.loc[allocation["Group"] == "Fund A", "Gains"].iloc[0] == 20
    assert contributors["bottom"].loc[0, "Gains"] == -10


def test_diagnostics_count_unmatched_funds_and_switches():
    dt = pd.DataFrame({"fund": ["Fund A", "Fund A", "Fund B"], "description": ["Purchase", "Switch In", "Purchase"], "amt": [1, 1, 1]})
    diag = build_quality_diagnostics(
        dt,
        {"Fund A": "123", "Fund B": None},
        pd.DataFrame({"Fund": ["Fund A", "Fund B"], "Category": ["Equity Scheme", None], "SubCategory": ["Large Cap Fund", None]}),
        pd.DataFrame({"Fund": ["Fund A", "Fund B"], "Source": ["Cache", "No match"]}),
    )
    assert diag.loc[diag["Check"] == "Unmatched NAV schemes", "Count"].iloc[0] == 1
    assert diag.loc[diag["Check"] == "Switch transactions", "Count"].iloc[0] == 1
