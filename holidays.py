import pandas as pd

easter_sunday = pd.DataFrame(
    {
        "holiday": "easterSunday",
        "ds": pd.to_datetime(
            [
                "2010-04-04",
                "2011-04-24",
                "2012-04-08",
                "2013-03-31",
                "2014-04-20",
                "2015-04-05",
                "2016-03-27",
                "2017-04-16",
                "2018-04-01",
                "2019-04-21",
                "2020-04-12",
                "2021-04-04",
                "2022-04-17",
                "2023-04-09",
                "2024-03-31",
            ]
        ),
        "lower_window": -2,
        "upper_window": 0,
    }
)

memorial_day = pd.DataFrame(
    {
        "holiday": "memorialMonday",
        "ds": pd.to_datetime(
            [
                "2010-05-31",
                "2011-05-30",
                "2012-05-28",
                "2013-05-27",
                "2014-05-26",
                "2015-05-25",
                "2016-05-30",
                "2017-05-29",
                "2018-05-28",
                "2019-05-27",
                "2020-05-25",
                "2021-05-31",
                "2022-05-30",
                "2023-05-29",
                "2024-05-27",
            ]
        ),
        "lower_window": -2,
        "upper_window": 0,
    }
)

labor_day = pd.DataFrame(
    {
        "holiday": "laborMonday",
        "ds": pd.to_datetime(
            [
                "2010-09-6",
                "2011-09-5",
                "2012-09-3",
                "2013-09-2",
                "2014-09-1",
                "2015-09-7",
                "2016-09-5",
                "2017-09-4",
                "2018-09-3",
                "2019-09-2",
                "2020-09-7",
                "2021-09-6",
                "2022-09-5",
                "2023-09-4",
                "2024-09-2",
            ]
        ),
        "lower_window": -2,
        "upper_window": 0,
    }
)

thanksgiving = pd.DataFrame(
    {
        "holiday": "thanksgiving",
        "ds": pd.to_datetime(
            [
                "2010-11-25",
                "2011-11-24",
                "2012-11-22",
                "2013-11-28",
                "2014-11-27",
                "2015-11-26",
                "2016-11-24",
                "2017-11-23",
                "2018-11-22",
                "2019-11-28",
                "2020-11-26",
                "2021-11-25",
                "2022-11-24",
                "2023-11-23",
                "2024-11-28",
            ]
        ),
        "lower_window": 0,
        "upper_window": 1,
    }
)

ascension_day = pd.DataFrame(
    {
        "holiday": "ascensionDay",
        "ds": pd.to_datetime(
            [
                "2014-05-29",
                "2015-05-14",
                "2016-05-05",
                "2017-05-25",
                "2018-05-10",
                "2019-05-30",
                "2020-05-21",
                "2021-05-13",
                "2022-05-26",
                "2023-05-18",
                "2024-05-09",
            ]
        ),
        "lower_window": 0,
        "upper_window": 2,
    }
)

whit_monday = pd.DataFrame(
    {
        "holiday": "whitMonday",
        "ds": pd.to_datetime(
            [
                "2014-06-09",
                "2015-05-25",
                "2016-05-16",
                "2017-06-05",
                "2018-05-21",
                "2019-06-10",
                "2020-06-01",
                "2021-05-24",
                "2022-06-06",
                "2023-05-29",
                "2024-05-20",
            ]
        ),
        "lower_window": -1,
        "upper_window": 0,
    }
)

may_day = pd.DataFrame(
    {
        "holiday": "mayday",
        "ds": pd.to_datetime(
            [
                "2015-05-04",
                "2016-05-02",
                "2017-05-01",
                "2018-05-07",
                "2019-05-06",
                "2020-05-04",
                "2021-05-03",
                "2022-05-02",
                "2023-05-01",
                "2024-05-06",
                "2025-05-05",
            ]
        ),
        "lower_window": -1,
        "upper_window": 0,
    }
)

spring_bank = pd.DataFrame(
    {
        "holiday": "springbank",
        "ds": pd.to_datetime(
            [
                "2015-05-25",
                "2016-05-30",
                "2017-05-29",
                "2018-05-28",
                "2019-05-27",
                "2020-05-25",
                "2021-05-31",
                "2022-05-30",
                "2023-05-29",
                "2024-05-27",
                "2025-05-26",
            ]
        ),
        "lower_window": -1,
        "upper_window": 0,
    }
)

summer_bank = pd.DataFrame(
    {
        "holiday": "summerbank",
        "ds": pd.to_datetime(
            [
                "2015-08-31",
                "2016-08-29",
                "2017-08-28",
                "2018-08-27",
                "2019-08-26",
                "2020-08-31",
                "2021-08-30",
                "2022-08-29",
                "2023-08-28",
                "2024-08-26",
                "2025-08-25",
            ]
        ),
        "lower_window": -1,
        "upper_window": 0,
    }
)


HOLIDAYS_AMR = pd.concat(
    [easter_sunday, memorial_day, labor_day, thanksgiving]
)
HOLIDAYS_CE = pd.concat([easter_sunday, ascension_day, whit_monday])
HOLIDAYS_FR = pd.concat([easter_sunday, ascension_day, whit_monday])
HOLIDAYS_SOEU = pd.concat([easter_sunday])
HOLIDAYS_UK = pd.concat([easter_sunday, may_day, spring_bank, summer_bank])
