# Bu fonksiyon ilk önce iki kullanıcı arasında bir boşluk kalacak şekilde, bu tip koltukların tamamı dolduktan sonra da kalan koltuklar arasından rastlantısal olarak koltuk numarası belirler.

import numpy as np

def seagap(sira_sayisi=10, kullanici_sayisi=10*sira_sayisi, seed=321, verbose=True):

  # Add controls later

  #seed=321
  #sira_sayisi = 10
  rg = np.random.default_rng(seed)
  #kullanici_sayisi = 100
  n_row = sira_sayisi
  n_users = kullanici_sayisi

  block1 = np.arange(1, n*5 + 1, 1)
  block2 = np.arange(n*5 + 1, 15*n + 1, 1)
  block3 = np.arange(15*n + 1, 20*n + 1, 1)
  block1_3 = np.hstack((block1, block3))

  seat_cap = 20 * n_row

  # başlamak için tümüyle boş salon
  seats = np.hstack((block1, block2, block3))
  seat_status = np.array([True] * seat_cap)

  # Mod 5'e göre 1, 3 ve 0 olan beşli blok koltuk numaraları
  block1_3_mod5 = block1_3[np.isin(block1_3 % 5, [0, 1, 3])]
  # Mod 5'e göre 2, 4, 6, 8 ve 0 olan onlu blok koltuk numaraları
  # Ya da çift sayılar
  block2_mod10 = block2[block2 % 2 == 0]

  seats_mod5_10 = np.hstack((block1_3_mod5, block2_mod10))
  max_empty = len(seats_mod5_10)

  # Koltuk seçme fonksiyonları
  def pick_seat(mod5_10_seat_status, type='mod5_10'):
    """
    Koltuk seçen fonksiyon.
    """
    if type == "mod5_10":
      empty_seats_mod5_10 = seats_mod5_10[mod5_10_seat_status]
      if (len(empty_seats_mod5_10) > 1):
        seat_pick = empty_seats_mod5_10[rg.integers(0, len(empty_seats_mod5_10), 1)][0]
      else:
        seat_pick = empty_seats_mod5_10[0]
      return seat_pick
    else:
      empty_seats = seats[seat_status]
      if (len(empty_seats) > 1):
        return empty_seats[rg.integers(0, len(empty_seats), 1)][0]
      else:
        return empty_seats[0]

  def seat_picker():
    """
    Koltuk seçme talimatı veren fonksiyon.
    """
    if sum(seat_status) > 0:
      mod5_10_seat_status = seat_status[seats_mod5_10 - 1]
      if sum(mod5_10_seat_status) > 0:
        seat_pick = pick_seat(mod5_10_seat_status)
      else:
        seat_pick = pick_seat(mod5_10_seat_status, type='any')
      return seat_pick
    else:
      return -1

  if verbose:
    print(50 * "*")
    print("Toplam başlangıç boş koltuk sayısı: ", seat_cap)
    print("Bir boşluklu maximum koltuk sayısı: ", max_empty)

  # Koltuk seçme
  seat_order = [np.nan] * seat_cap

  if n_users != 0:
    for user in range(1, n_users + 1, 1):
      seat_pick = seat_picker()
      
      if (seat_pick > 0):
        seat_status[seat_pick - 1] = False
        
        if verbose:
          print(user, ":", "Koltuk numaranız: ", seat_pick, sep="")

        if user <= seat_cap:
          seat_order[user - 1] = seat_pick
          if verbose:
            if user == max_empty:
              print(50 * "*")
              print("1. ve 3. bloklarda mod 5'e göre 0, 1 ve 3 olan ve", "2. blokta mod 10'a göre 0, 2, 4, 6 ve 8 olan", "koltukların tamamı dolu!", "Diğer numaralara geçiliyor...", sep = "\n")
              print(50 * "*") 
        else:
          print("200'den fazla kullanici var!!!!")
          if verbose:
            print(50 * "*")
            print(user, ": ", "Üzgünüz, şu anda hiç boş koltuğumuz yok.", sep = "")

  if verbose:
    print("Kalan boş koltuk sayısı:", sum(seat_status))
