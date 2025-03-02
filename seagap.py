# Bu fonksiyon ilk önce iki kullanıcı arasında bir boşluk kalacak şekilde, bu tip koltukların tamamı dolduktan sonra da kalan koltuklar arasından rastlantısal olarak koltuk numarası belirler.

import numpy as np

def seagap(sira_sayisi=10, kullanici_sayisi=10*sira_sayisi, seed=321, verbose=True):

  # Add controls later

  sira_sayisi = 10
  kullanici_sayisi = 100
  n = sira_sayisi
  users = kullanici_sayisi

  block1 = [x for x in range(1, n * 5 + 1)]
  block2 = [x for x in range(n * 5 + 1, 15 * n + 1)]
  block3 = [x for x in range(15 * n + 1, 20 * n + 1)]
  block1_3 = block1 + block3

  seat_cap = 20 * n

  # başlamak için tümüyle boş salon
  seats = block1 + block2 + block3
  seat_status = [True] * seat_cap

  # Mod 5'e göre 1, 3 ve 0 olan beşli blok koltuk numaraları
  seats_mod5 = block1_3[(np.array(block1_3) % 5).isin([0, 1, 3])]
  # Mod 5'e göre 2, 4, 6, 8 ve 0 olan onlu blok koltuk numaraları
  # Ya da çift sayılar
  seats_mod10 = block2[np.array(block2) % 2 == 0]
