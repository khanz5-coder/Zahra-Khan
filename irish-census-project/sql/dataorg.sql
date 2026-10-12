SELECT sex, marst, age, countyie,

COUNT(*) AS n,
100.0 * SUM(CASE WHEN lit = 2 THEN 1 ELSE 0 END) / COUNT(*) AS literacy_rate 

-- creating a literacy rate column by calculating the percentage of individuals in each literacy category within each county --

FROM persons_raw

WHERE year = 1911
    AND lit <> 9
    AND sex IN (1,2)
    AND age <> 999
    AND age BETWEEN 13 AND 99
    AND marst IN (1,2,3,4)

GROUP BY countyie, sex, age, marst
ORDER BY literacy_rate DESC;
