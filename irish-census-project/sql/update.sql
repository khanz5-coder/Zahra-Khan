SELECT countyie, age, marst, sex, lit,

    COUNT(*) AS n,
    100.0 * SUM(CASE WHEN lit = 2 THEN 1 ELSE 0 END) / COUNT(*) AS literacy_rate

FROM persons_raw

WHERE year = 1911
    AND lit IN (1,2)
    AND age >= 13
    AND age <= 80
    AND sex IN (1,2)
    AND marst IN (1,2,3,4)

GROUP BY countyie, sex, age, marst, lit

ORDER BY literacy_rate DESC;