# Bài toán: Giả sử bạn đang nghiên cứu về mối quan hệ giữa tỷ lệ tiêu thụ năng lượng của 1 căn hộ (dependent variable, Y) và các biến độc lập
# là diện tích căn hộ (X₁), số phòng ngủ (X₂), và khoảng cách từ trung tâm thành phố (X₃)
data = read.csv("/Users/vudung/Downloads/du-lieu.csv")
data

#Xem thử tương quan của từng biến ngẫu nhiên với biến phụ thuộc 
par(mfrow = c(1,3))
plot(data$Area,data$energy.consumption,pch = 16, col = 'blue', main = "Area vs Energy") #tương quan thuận
plot(data$num_bedroom, data$energy.consumption,pch = 16, col = 'blue', main = "Number of Bedroom vs Energy") #khá rời rạc 
plot(data$distance, data$energy_consumption, pch = 16, col = 'blue', main = "Distance vs Energy") #tương quan nghịch 

par(mfrow = c(1,1))

#Xây dựng mô hình 
model.energy <- lm(energy_consumption ~ Area + num_bedroom + distance, data = data)
summary(model.energy)

# Kết quả: energy_consumption = 1.48960 + 2.03372*Area + 4.21297*num_bedroom - 0.09815*distance

# Tìm khoảng tin cậy cho các hệ số hồi quy: 
confint(model.energy)


