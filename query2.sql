WITH orders_data AS(
  SELECT 
    city,
    user_id,
    COUNT(order_id) AS orders,
    ROW_NUMBER() OVER(PARTITION BY city ORDER BY COUNT(order_id) DESC) row_id
  FROM `efood2022-379301.main_assessment.orders`
  GROUP BY 1,2
  ORDER BY 1,4
)
SELECT 
  city,
  SUM(CASE WHEN row_id<11 THEN orders END) top_10_users_orders,
  SUM(orders) total_orders,
  SUM(CASE WHEN row_id<11 THEN orders END)/SUM(orders) contibute_of_orders
FROM orders_data
GROUP BY 1
ORDER BY 2 DESC
