import numpy as np
from scipy.optimize import milp, LinearConstraint, Bounds

# --------------------------
# Model data (tiny example)
# --------------------------
J = 1          # one synchronous tech
T = 2          # two periods
B = 2          # two build blocks
K = 1          # one inertia device

S_exist = np.array([100.0])     # MW existing synchronous unit
H = np.array([6.0])             # inertia constant (s) for tech
block_size = 50.0               # MW per build block
I = np.array([3.0])             # inertia per MW of syncon device

P_loss = np.array([150.0, 150.0])
RoCoF_max = 1.0
Hmin = P_loss / (2 * RoCoF_max)   # vector length T

demand = np.array([120.0, 130.0])

# Costs
capex_block = 200
fixed_cost = 5
var_cost = 20
capex_syncon = 300

# --------------------------
# Variable ordering in x
# --------------------------
# b_jb (J*B ints), y_jt (J*T ints), p_jt (J*T cont), z_k (K cont)
n_b = J * B
n_y = J * T
n_p = J * T
n_z = K
n_vars = n_b + n_y + n_p + n_z

def idx_b(j, b):
    return j*B + b

def idx_y(j, t):
    return n_b + j*T + t

def idx_p(j, t):
    return n_b + n_y + j*T + t

def idx_z(k):
    return n_b + n_y + n_p + k

# --------------------------
# Objective vector c
# --------------------------
c = np.zeros(n_vars)

# capex for each block
for j in range(J):
    for b in range(B):
        c[idx_b(j,b)] = capex_block

# fixed cost for y
for j in range(J):
    for t in range(T):
        c[idx_y(j,t)] = fixed_cost

# var cost for p
for j in range(J):
    for t in range(T):
        c[idx_p(j,t)] = var_cost

# capex for syncon
for k in range(K):
    c[idx_z(k)] = capex_syncon

# --------------------------
# Bounds and integrality
# --------------------------
lb = np.zeros(n_vars)
ub = np.full(n_vars, np.inf)

# build blocks 0/1
for j in range(J):
    for b in range(B):
        ub[idx_b(j,b)] = 1.0

# y binary
for j in range(J):
    for t in range(T):
        ub[idx_y(j,t)] = 1.0

bounds = Bounds(lb, ub)

integrality = np.zeros(n_vars, dtype=int)
integrality[:n_b] = 1                # build blocks → integer
integrality[n_b:n_b+n_y] = 1         # y is binary
# p and z continuous (default 0)

# --------------------------
# Constraint list
# --------------------------
constraints = []

# (1) Power balance for each t
for t in range(T):
    row = np.zeros(n_vars)
    for j in range(J):
        row[idx_p(j,t)] = 1.0
    constraints.append(
        LinearConstraint(row, demand[t], demand[t])
    )

# (2) Dispatch limit p_jt <= S_exist*y + block_size*sum b
for j in range(J):
    for t in range(T):
        row = np.zeros(n_vars)
        # p term
        row[idx_p(j,t)] = 1.0
        # -S_exist*y
        row[idx_y(j,t)] = -S_exist[j]
        # -block_size * sum blocks
        for b in range(B):
            row[idx_b(j,b)] = -block_size

        constraints.append(
            LinearConstraint(row, -np.inf, 0.0)
        )

# (3) Inertia constraint per t:
# sum H_j*(S_exist + block_size*sum b)*y_jt + I*z >= Hmin
# (We linearise conservatively: treat block_size*sum(b) as always available.)
for t in range(T):
    row = np.zeros(n_vars)

    for j in range(J):
        # coefficient on y_jt: H * S_exist
        row[idx_y(j,t)] = - H[j] * S_exist[j]

        # coefficient on blocks: H * block_size
        for b in range(B):
            row[idx_b(j,b)] = - H[j] * block_size

    # syncon z term
    for k in range(K):
        row[idx_z(k)] = - I[k]

    constraints.append(
        LinearConstraint(row, -np.inf, -Hmin[t])
    )

# --------------------------
# Solve MILP
# --------------------------
res = milp(c=c,
           integrality=integrality,
           bounds=bounds,
           constraints=constraints,
           options={"disp": True})

print("Status:", res.status)
print("Message:", res.message)
print("Objective:", res.fun)
print("Solution:", res.x)
