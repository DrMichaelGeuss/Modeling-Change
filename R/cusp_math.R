# -----------------------------
# Cusp catastrophe: potential + equilibria
# -----------------------------

# V(z) = (1/4) z^4 - (1/2) beta z^2 - alpha z
V_potential <- function(z, alpha, beta) {
  0.25 * z^4 - 0.5 * beta * z^2 - alpha * z
}

# V'(z) = z^3 - beta z - alpha
V_prime <- function(z, alpha, beta) {
  z^3 - beta * z - alpha
}

# V''(z) = 3 z^2 - beta
V_second <- function(z, beta) {
  3 * z^2 - beta
}

# Real roots of z^3 - beta z - alpha = 0
cusp_equilibria <- function(alpha, beta, tol = 1e-8) {
  roots <- polyroot(c(-alpha, -beta, 0, 1))
  real_roots <- Re(roots)[abs(Im(roots)) < tol]
  sort(real_roots)
}

classify_equilibria <- function(z_roots, alpha, beta) {
  if (length(z_roots) == 0) {
    return(data.frame(z = numeric(), V = numeric(), V2 = numeric(), type = character()))
  }
  Vvals <- V_potential(z_roots, alpha, beta)
  V2 <- V_second(z_roots, beta)
  type <- ifelse(V2 > 0, "stable (min)", "unstable (max)")
  data.frame(z = z_roots, V = Vvals, V2 = V2, type = type, stringsAsFactors = FALSE)
}

# -----------------------------
# Statistical instantiation: 2D change equations (phase space)
# ΔX = a0 + a1*X + a2*Y
# ΔY = b0 + b1*X + b2*Y
# -----------------------------

dX <- function(x, y, a0, a1, a2) {
  a0 + a1 * x + a2 * y
}

dY <- function(x, y, b0, b1, b2) {
  b0 + b1 * x + b2 * y
}
