set.seed(345)

#num_of_line_five = 1
#num_of_line_ten = 0
seat_cap = 10

# arada bir boşluk olan maximum boş koltuk sayısı 
# max_empty = if (seat_cap %% 2 == 1) {
#   seat_cap * 0.6
# } else {
#   seat_cap * 0.5
# }

# tümüyle boş salon
seat_status = rep(TRUE, times = seat_cap)
seat_numbers = c(1:seat_cap)

# Mod 5'e göre 1, 3, ve 0 olan koltuk numaraları; daha sonra!!!
odd_seats = seq(1, seat_cap, 2)
max_empty = length(odd_seats)

# Koltuk seçme fonksiyonu
pick_seat = function(type = "odd", odd_seat_status) {

  if (type == "odd") {
    #odd_seat_status = seat_status[odd_seats]
    empty_odd_seats = odd_seats[odd_seat_status]

    if (length(empty_odd_seats) > 1) {
      seat_pick = sample(empty_odd_seats, 1)
    } else {
      seat_pick = empty_odd_seats[1]
    }

    return(seat_pick)
  } else {

    empty_seats = seat_numbers[seat_status]

    if (length(empty_seats) > 1) {
      return(sample(empty_seats, 1))
    } else {
      return(empty_seats[1])
    }
  }
}

seat_picker = function() {
  if (sum(seat_status) > 0) {

    odd_seat_status = seat_status[odd_seats]

    if (sum(odd_seat_status) > 0) {
      seat_pick = pick_seat("odd", odd_seat_status)
    } else {
      seat_pick = pick_seat("any")
    }
    return(seat_pick)
  } else {
    return(-1)
  }
}

cat("****************************************\n")
cat("Toplam koltuk sayısı: ", seat_cap, "\n")
cat("Bir boşluklu maximum koltuk sayısı: ", max_empty, "\n")
cat("****************************************\n")

for (i in 1:(seat_cap + 1)) {
  seat_pick = seat_picker()

  if (seat_pick > 0) {
    seat_status[seat_pick] = FALSE
    cat(i, ": ", "Koltuk numaranız: ", seat_pick, "\n", sep = "")

    if (i == max_empty) {
      cat("****************************************\n")
    }
  } else {
    cat("****************************************\n")
    cat(i, ": ", "Üzgünüz, şu anda hiç boş koltuğumuz yok.", "\n", sep = "")
  }
}

cat(paste("Kalan koltuk sayısı: ", sum(seat_status)))