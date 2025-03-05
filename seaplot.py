import matplotlib.pyplot as plt

# def seaplot(seating_data):

fig, ax = plt.subplots()

ax.scatter(
  "seat_x_grid", "seat_y_grid", data=grid, marker="s",
  c="gray")

for x, y, z in zip(grid.seat_x_grid, grid.seat_y_grid, grid.seats):
  ax.annotate("%s" %z, xy=(x, y), xytext=(x, y), textcoords='data')


ax.scatter("seat_x", "seat_y", data=grid, marker="s",
c="rosybrown")

ax.set_title("Raslantısal oturma deseni")