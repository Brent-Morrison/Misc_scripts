import numpy as np
from scipy.optimize import milp, LinearConstraint, Bounds

# --------------------------
# Variable ordering:
# z = [x1, x2, x3, y0, y1, y2, y3]
# --------------------------

# Objective coefficients: maximize 5x1 + 8x2 + 6x3 - 5000y0 - 1000y1 - 1200y2 - 900y3
# SciPy minimizes, so multiply by -1
c = -np.array([5, 8, 6, -5000, -1000, -1200, -900], dtype=float)

# --------------------------
# Constraint matrix A and RHS b
# --------------------------

A = np.array([
    [1, 2, 1, -200,    0,    0,    0],   # machine-hours: x1 + 2x2 + x3 -200y0 ≤ 100
    [1, 0, 0,    0,  -70,    0,    0],   # product 1: x1 - 70y1 ≤ 30
    [0, 1, 0,    0,    0,  -80,    0],   # product 2: x2 - 80y2 ≤ 20
    [0, 0, 1,    0,    0,    0,  -90],   # product 3: x3 - 90y3 ≤ 10
], dtype=float)

b = np.array([100, 30, 20, 10], dtype=float)

# Build LinearConstraint object
constraint = LinearConstraint(A, -np.inf, b)

# --------------------------
# Variable bounds
# x1,x2,x3 ≥ 0 ; y0,y1,y2,y3 ∈ {0,1}
# --------------------------

lower_bounds = [0, 0, 0, 0, 0, 0, 0]
upper_bounds = [np.inf, np.inf, np.inf, 1, 1, 1, 1]
bounds = Bounds(lower_bounds, upper_bounds)

# Specify integer variables (indices of y0, y1, y2, y3)
integrality = np.array([0, 0, 0, 1, 1, 1, 1], dtype=int)

# --------------------------
# Solve MILP
# --------------------------

res = milp(c=c, constraints=[constraint], bounds=bounds, integrality=integrality)

print(res)
print("Optimal decision variables z* = ", res.x)

# Recover variables by name:
x1, x2, x3, y0, y1, y2, y3 = res.x
print(f"x = {(x1, x2, x3)}")
print(f"y = {(y0, y1, y2, y3)}")
