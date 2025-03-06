import numpy as np
import pandas as pd

def seagap(sira_sayisi=10, kullanici_sayisi=10*10, seed=321, verbose=True):
  """
  Bu fonksiyon ilk önce iki kullanıcı arasında bir boşluk kalacak şekilde, bu tip koltukların tamamı dolduktan sonra da kalan koltuklar arasından rastlantısal olarak koltuk numarası belirler.
  """

  if sira_sayisi <= 0:
    raise ValueError('Pozitif tam sayı bekliyordum.')
  elif kullanici_sayisi < 0:
    raise ValueError('Negatif olmayan tam sayı bekliyordum.')
  elif not isinstance(kullanici_sayisi, int) or not isinstance(sira_sayisi, int):
    raise TypeError('Pozitif tam sayı bekliyordum.')
  elif not isinstance(verbose, bool):
    raise TypeError('True ya da False bekliyordum.')

  rg = np.random.default_rng(seed)
  n_row = sira_sayisi
  n_users = kullanici_sayisi

  block1 = np.arange(1, n_row * 5 + 1)
  block2 = np.arange(n_row * 5 + 1, 15 * n_row + 1)
  block3 = np.arange(15 * n_row + 1, 20 * n_row + 1)
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

  seats_w_gap = np.hstack((block1_3_mod5, block2_mod10))
  max_empty = len(seats_w_gap)

  # Koltuk seçme fonksiyonları
  def pick_seat(gap_status, gap):
    """
    Raslantısal koltuk seçen fonksiyon.

    Eğer gap=True ise, bir boşluk bırakacak şekilde, değilse herhangi bir koltuk numarası seçer. 
    """
    if gap:
      empty_seats_w_gap = seats_w_gap[gap_status]
      if (len(empty_seats_w_gap) > 1):
        seat_pick = empty_seats_w_gap[rg.integers(0, len(empty_seats_w_gap), 1)][0]
      else:
        seat_pick = empty_seats_w_gap[0]
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

    Boş koltuk varsa koltuk seçmek için pick_seat() fonksiyonunu çağırır ve seçilen koltuk numarasını geri gönderir. Yoksa -1 geri gönderir.
    """
    if sum(seat_status) > 0:
      gap_status = seat_status[seats_w_gap - 1]
      if sum(gap_status) > 0:
        seat_pick = pick_seat(gap_status, gap=True)
      else:
        seat_pick = pick_seat(gap_status, gap=False)
      return seat_pick
    else:
      return -1

  if verbose:
    print(50 * "*")
    print("Toplam başlangıç boş koltuk sayısı: ", seat_cap)
    print("Bir boşluklu maximum koltuk sayısı: ", max_empty)
    print("Kullanıcı sayısı: ", n_users)
    print(50 * "*")

  # Seçilen koltukları tutacak liste
  seat_order = [np.nan] * seat_cap

  # Sıfırdan büyük kullanıcı sayıları için koltuk seçme
  if n_users != 0:
    for user in range(1, n_users + 1):
      seat_pick = seat_picker()
      if seat_pick > 0:
        seat_status[seat_pick - 1] = False
        if verbose:
          print(user, ". kullanıcı", " --> ", "Koltuk numaranız: ", seat_pick, sep="")
          if user == seat_cap:
            print("Kalan boş koltuk sayısı:", sum(seat_status))
        if user <= seat_cap:
          seat_order[user - 1] = seat_pick
          if verbose:
            if user == max_empty:
              print(50 * "*")
              print("1. ve 3. bloklarda mod 5'e göre 0, 1 ve 3 olan ve", "2. blokta mod 10'a göre 0, 2, 4, 6 ve 8 olan", "koltukların tamamı dolu!", "Diğer numaralara geçiliyor...", sep = "\n")
              print(50 * "*")
      elif seat_pick == -1:
        if verbose:
          if user == seat_cap + 1:
            print(50 * "*")
          print(user, ": ", "Üzgünüz, şu anda hiç boş koltuğumuz yok.", sep = "")

  def situate(seats):
    """Grid ve oturma verisi oluşturma fonksiyonu"""

    seat_x = np.array([np.nan] * len(seats))

    for i in range(len(seats)):
      if not np.isnan(seats[i]):
        if np.isin(seats[i], block1):
          seat_x[i] = seats[i] % 5
          if seat_x[i] == 0:
            seat_x[i] = 5
        elif np.isin(seats[i], block2):
          seat_x[i] = seats[i] % 10
          if n_row % 2 == 0:
            if seat_x[i] == 0:
              seat_x[i] = 10
          else:
            if seat_x[i] >= 6:
              seat_x[i] = seat_x[i] % 5
            else:
              seat_x[i] += 5
          seat_x[i] += 6
        else:
          seat_x[i] = seats[i] % 5
          if seat_x[i] == 0:
            seat_x[i] = 5
          seat_x[i] += 17

    seat_y = np.array([np.nan] * len(seats))

    for i in range(len(seats)):
      if not np.isnan(seats[i]):
        if np.isin(seats[i], block1):
          for j in range(1, 1 + n_row):
            in_interval = (seats[i] > (j - 1) * 5) and (seats[i] <= j * 5)
            if in_interval:
              seat_y[i] = j
        elif np.isin(seats[i], block2):
          for j in range(1, 1 + n_row):
            in_interval = (seats[i] > 5 * n_row + (j - 1) * 10) and (seats[i] <= 5 * n_row + j * 10)
            if in_interval:
              seat_y[i] = j
        else:
          for j in range(1, 1 + n_row):
            in_interval = (seats[i] > 15 * n_row + (j - 1) * 5) and (seats[i] <= 15 * n_row + j * 5)
            if in_interval:
              seat_y[i] = j

    return pd.DataFrame(
      {
        "seats": seats,
        "seat_x": seat_x,
        "seat_y": seat_y,
      },
      dtype=pd.Int64Dtype()
    )

  grid = situate(seats)
  grid.rename(columns={"seat_x": "seat_x_grid", "seat_y": "seat_y_grid"},inplace=True)

  seating_data = situate(seat_order)
  seating_data.rename(columns={"seats": "seat_order"}, inplace=True)

  return pd.concat([seating_data, grid], axis=1)