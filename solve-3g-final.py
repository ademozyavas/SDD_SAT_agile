from pysat.formula import WCNF
from pysat.examples.rc2 import RC2

# ------------------------------------------------------------
# Read WCNF
# ------------------------------------------------------------

wcnf = WCNF("3g-final.wcnf")

print("========================================")
print("3G MAX-SAT")
print("========================================")
print("Variables:", wcnf.nv)
print("Hard clauses:", len(wcnf.hard))
print("Soft clauses:", len(wcnf.wght))
print("========================================")

# ------------------------------------------------------------
# Solve
# ------------------------------------------------------------

with RC2(wcnf) as rc2:
    model = rc2.compute()
    cost = rc2.cost

total_soft_weight = sum(wcnf.wght)
optimal_value = total_soft_weight - cost

print("\nOptimal solution found.")
print("Cost:", cost)
print("Total soft weight:", total_soft_weight)
print("Optimal objective:", optimal_value)

# ------------------------------------------------------------
# Save model
# ------------------------------------------------------------

with open("3g-final.model", "w") as f:
    for literal in model:
        f.write(f"{literal} ")

    f.write("0\n")

print("\nModel written to 3g-final.model")
