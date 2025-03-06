import matplotlib.pyplot as plt

def seaplot(seating_data):
  """Koltuk numaralarını görselleştiren fonksiyon"""
  
  fig, ax = plt.subplots()

  ax.set_facecolor("lightgrey")
  ax.tick_params(
    axis='both',
    which='both',
    bottom=False,
    left=False,
    labelbottom=False,
    labelleft=False
  )
  ax.set_title("Raslantısal oturma deseni - " + 
               str(seating_data.   seat_order.count()) + 
               " kullanıcı")

  ax.scatter("seat_x_grid", "seat_y_grid", data=seating_data, marker="s", 
             s=400, facecolor="lightgrey", edgecolors="black")

  ax.scatter("seat_x", "seat_y", data=seating_data, marker="s", 
             s=400, facecolor="rosybrown", edgecolors="black")

  for x, y, s in zip(seating_data.seat_x_grid, seating_data.seat_y_grid, seating_data.seats):
    ax.text(x, y, s, ha="center", va="center")