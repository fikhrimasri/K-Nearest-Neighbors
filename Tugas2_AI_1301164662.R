train <- read.csv("E:/Matkul kuliah/Semester 6/AI/Tugas 2 AI/Dataset/DataTrain Tugas 2 AI.csv", sep = ",")
test <- read.csv("E:/Matkul kuliah/Semester 6/AI/Tugas 2 AI/Dataset/DataTest_Tugas_2_AI.csv", sep = ",")

#Fungsi dari distance untuk menghitung jarak antar attribut
distance <- function(x1, x2){
  temp = 0
  for(i in c(1:(length(x1)-1) ))
  {
    temp = temp + (x1[[i]]-x2[[i]])^2
  }
  temp = sqrt(temp)
  return(temp)
}

#Fungsi KNN
K.Nearest.Neighbor <- function(valid, train, k){
  # Inisialisasi vector untuk menyimpan prediksi kelas
  rowclass <- c()
  for(j in 1:nrow(valid)){
    # Gabungkan label asli dari datatest dengan distance menjadi satu dataframe 
    rowvalue <- data.frame("dist" = distance(valid[j,], train), "kelas" = train$kelas)
    
    # Urutkan dataframe berdasarkan jarak secara ascending
    rowvalue <- rowvalue[order(rowvalue$dist),]
    
    # Ambil hasil urut dari 1 sampai k dan hitung frekuensi kemunculan label yang ada
    predict <- as.data.frame(table(rowvalue[1:k,]$kelas))
    # Urutkan dataframe label berdasarkan frekuensi secara DESCENDING 
    predict <- predict[order(predict$Freq, decreasing = TRUE),]
    
    # Append ke vector label dengan frekuensi maksimal
    rowclass <- c(rowclass, as.numeric(as.character(predict$Var1[1])))
  }
  
  # Hitung akurasi antara vector prediksi label dengan label asli
  acc <- rowclass == valid$kelas
  return((length(acc[acc == TRUE]) / length(acc)) * 100)
}


# Pencarian K terbaik
K <-60
fold <- 5
split.data <- nrow(train) / fold


result <- c()
for(i in 1:fold){
  # Ambil data validation dan train baru untuk validasi
  test.validation <- train[c((1 + split.data*(i-1)) : (split.data*i)),]
  train.validation <- train[-c((1 + split.data*(i-1)) : (split.data*i)),]
  
  # inisialisasi vector untuk Tunning Hyperparameter K
  accuracy.per.test <- c()
  for(k in 1:K){
    # Menghitung akurasi tiap K
    accuracy.per.test <- c(accuracy.per.test, K.Nearest.Neighbor(test.validation, train.validation, k))
  }
  
  result <- rbind(result, accuracy.per.test)
}
train.validation
test.validation
colnames(result) <- 1:K

# Menghitung rata - rata dari semua lipatan setiap K
result <- colSums(result) / nrow(result)
result

# Ambil K dengan akurasi terbaik
K.Terbaik <- as.numeric(names(result[result == max(result)]))
K.Terbaik

plot(result, type = "b", col = "red", col.axis="red", main  = "KNN Classification", xlab = "K Value", ylab = "Accuracy")

rowclass <- c()
for(j in 1:nrow(test)){
  rowvalue <- data.frame("dist" = distance(test[j,], train), "kelas" = train$kelas)
  rowvalue <- rowvalue[order(rowvalue$dist),]

  class.predict <- as.data.frame(table(rowvalue[1:K.Terbaik,]$kelas))
  class.predict <- class.predict[order(class.predict$Freq, decreasing = TRUE),]

  rowclass <- c(rowclass, as.numeric(as.character(class.predict$Var1[1])))
}

rowclass
write.table(rowclass, file = "E:/Matkul kuliah/Semester 6/AI/Tugas 2 AI/Predicttugas2.csv", sep = ",", row.names = FALSE, col.names = "kelas")

