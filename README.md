# InstaCartMarketBasketAnalysis

Big Data Project

# Survey & Related Approaches
Instacart Market Basket Analysis (Kaggle Competition) https://rpubs.com/Guy_Larange/283625
https://github.com/swapnil2129/Kaggle-Competition-Instacart-EDA-MBA


# Some Features to be added or exploited
Copied from this blog post, All credits to the blog author.

link http://blog.kaggle.com/2017/09/21/instacart-market-basket-analysis-winners-interview-2nd-place-kazuki-onodera/

### Feature Engineering

User features - what is this user like?
Item features - what is this item like?
User x item features - how does this user feel about this item?
Datetime features - what is this day and hour like?
Here are some of the ideas behind the features I created.

#### User features

How often the user reordered items
Time between orders
Time of day the user visits
Whether the user ordered organic, gluten-free, or Asian items in the past
Features based on order sizes
How many of the user’s orders contained no previously purchased items

#### Item features

How often the item is purchased
Position in the cart
How many users buy it as "one shot" item
Stats on the number of items that co-occur with this item
Stats on the order streak
Probability of being reordered within N orders
Distribution of the day of week it is ordered
Probability it is reordered after the first order
Statistics around the time between orders

#### User x Item features

Number of orders in which the user purchases the item
Days since the user last purchased the item
Streak (number of orders in a row the user has purchased the item)
Position in the cart
Whether the user already ordered the item today
Co-occurrence statistics
Replacement items

#### Datetime features

Counts by day of week
Counts by hour
