all <- \(x,y) {
  c(tau_weak = tau_weak(x,y),
    tau_strict = tau_strict(x,y),
    somers_d = somers_d(x,y))
}

all(x1, -y1)
all(x2, -y2)
all(x3, -y3)
all(x4, -y4)
all(x5, -y5)
all(x6, -y6)
all(x7, -y7)
all(x8, -y8)
all(x11, -y11)
