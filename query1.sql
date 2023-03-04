WITH orders_data AS(
  SELECT *
  FROM `efood2022-379301.main_assessment.orders`
),
cities AS(
  SELECT 
    city,
    COUNT(DISTINCT order_id) orders,
    COUNT(DISTINCT IF(cuisine='Breakfast',order_id , NULL)) breakfast_orders,
    COUNT(DISTINCT user_id) users,
    COUNT(DISTINCT IF(cuisine='Breakfast',user_id , NULL)) breakfast_users , 
    SUM(amount) amount,
    SUM(IF(cuisine='Breakfast',amount , NULL)) breakfast_amount
  FROM orders_data
  GROUP BY 1
  HAVING orders >1000
),
user_per_city_with3_orders AS(
  SELECT 
    city,
    COUNT(user_id) users_with_3,
    COUNT(IF(breakfast_flag =1 , user_id,NULL)) breakfast_users_with_3,
  FROM (  
    SELECT
      city,
      user_id,
      CASE WHEN cuisine = 'Breakfast' THEN 1 ELSE 0 END breakfast_flag,
      COUNT(order_id)/COUNT(DISTINCT user_id) freq,
    FROM orders_data
    GROUP BY 1,2,3
  )
  WHERE freq>3
  GROUP BY 1
),
agg AS(
  SELECT 
    c.city,
    c.orders,
    c.breakfast_orders,
    c.users,
    c.breakfast_users,
    c.amount,
    c.breakfast_amount,
    u.users_with_3,
    u.breakfast_users_with_3
  FROM cities AS c
  JOIN user_per_city_with3_orders AS u ON c.city=u.city
)
 SELECT 
    city ,
    breakfast_amount / breakfast_orders AS breakfast_basket , 
    amount / orders AS efood_basket ,
    breakfast_orders / breakfast_users AS breakfast_freq ,
    orders / users AS efood_freq ,
    breakfast_users_with_3 / breakfast_users AS breakfast_users_rate3,
    users_with_3 / users AS efood_users_rate3,
FROM agg
ORDER BY breakfast_orders DESC
LIMIT 5

-- Comments on results 
-- From the data derives that Rodos has the biggest mean basket amount both for breakfast and total efood orders per city. 
--Also, Rodos has the largest difference between the amount spent for breakfast compared to the total efood amount spent per city.
-- Volos has the smallest difference between frequency orders of breakfast and total frequency orders per city.
-- Xanthi seems to have the most "dedicated" users, having the biggest percentage of users with more than 3 orders per month in both categories (Total and breakfast).
-- As a general conclusion, we do not observe any distinct differences between cities, so customers seem to have same habits moreorless regardless of place.
