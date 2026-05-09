# Load packages
using Grumps
using CSV
using DataFrames

# Set working directory
cd("C:/Users/ryanh/Dropbox/admissions_project/data/")

# Need to get a product-level dataset, student-level dataset, and market-level dataset
# Product-level: o_share, ur, net_rate, IV
# Student-level: oos, choice (name of chosen state)
# Market-level: year
# Then need to make user-specified interactions for home-state preference

# Use mixed logit estimator at first
e = Estimator(:mixedlogit)

# Specify where to find the data
s = Sources(
    consumers = "students.csv",
    products = "destinations.csv"
)

# Need to figure out how to interact OOS with home-state as well...

# Specify the model
v = Variables(
    interactions = [
        :o_state :state;
        # Interacting OOS with the constant is equivalent to allowing in-state students' home-state preference to differ
        :oos :constant;
        #:o_state_oos :state;
        :oos :o_share;
        :oos :t_AL;
        ],
    randomcoefficients = [:constant],
    regressors = [:constant; :o_share; :t_AL],
    instruments = [:constant; :o_share; :t_AL],
    outsidegood = "Alabama",
    market = :market,
    product = :product
)

# Actually, I don't know if this way of specifying the home-state
# interaction will work. Home state isn't a product-specific
# characteristic - it varies at the product/student level.

# User-specified interactions
function InteractionsCallback(z, x, i, j, t, micmac, market, products)
    # Other interactions remain standard
    if t > 1
        return z[i, t] * x[j, t]
    end
    # For the first two interactions, get whether state j is student i's home state
    return z[i, t] == x[j, t]
end

# Get structured data
d = Data(e, s, v)

# Get solution
sol = grumps!(e, d)