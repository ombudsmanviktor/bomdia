alphav1 <- krippalpha(v1,
                    bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav2 <- krippalpha(v2,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav3 <- krippalpha(v3,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav4 <- krippalpha(v4,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav5 <- krippalpha(v5,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav6 <- krippalpha(v6,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav7 <- krippalpha(v7,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav8 <- krippalpha(v8,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav9 <- krippalpha(v9,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav10 <- krippalpha(v10,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav11 <- krippalpha(v11,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav12 <- krippalpha(v12,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav13 <- krippalpha(v13,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
alphav14 <- krippalpha(v14,
                      bootstrap = TRUE, bootnp = TRUE, cores = 2)
print(alphav1)
print(alphav2)
print(alphav3)
print(alphav4)
print(alphav5)
print(alphav6)
print(alphav7)
print(alphav8)
print(alphav9)
print(alphav10)
print(alphav11)
print(alphav12)
print(alphav13)
print(alphav14)

library(cowplot)

plot(alphav1)
plot(alphav2)
plot(alphav3)
plot(alphav4)
plot(alphav5)
plot(alphav6)
plot(alphav7)
plot(alphav8)
plot(alphav9)
plot(alphav10)
plot(alphav11)
plot(alphav12)
plot(alphav13)
plot(alphav14)

plot_grid(alphav1, alphav1, nrow = 2)

