# Programın görsel çıktıları

seapart(sira_sayisi =  10, kullanici_sayisi = 0) |>
  seaplot() + 
  labs(title = "200 koltuklu boş salon")
ggsave("empty_room.jpeg", width = 8, height = 5, units = "in")

seapart(sira_sayisi =  10, kullanici_sayisi = 70) |>
  seaplot()
ggsave("users_70.jpeg", width = 8, height = 5, units = "in")

seapart(sira_sayisi =  10, kullanici_sayisi = 90) |>
  seaplot()
ggsave("users_90.jpeg", width = 8, height = 5, units = "in")

seapart(sira_sayisi =  10, kullanici_sayisi = 110) |>
  seaplot()
ggsave("users_110.jpeg", width = 8, height = 5, units = "in")

seapart(sira_sayisi =  10, kullanici_sayisi = 200) |>
  seaplot()
ggsave("users_200.jpeg", width = 8, height = 5, units = "in")