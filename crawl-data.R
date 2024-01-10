install.packages("readr")
library(readr)
install.packages("lubridate")
library(lubridate)
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
library(ggthemes)
# Đọc file Excel (.xlsx)
dataframe_df <- read_csv("D:/work-space/ngon_ngu_R/Phan-tich-du-lieu-R/s-channel.csv")
head(dataframe_df)
# Loại bỏ cột 'channel_id' và 'video_id' từ dataframe_df
dataframe_df <- dataframe_df[, !(names(dataframe_df) %in% c('channel_id', 'video_id'))]



# Kiểm tra số hàng và số cột của dataframe_df
num_rows <- nrow(dataframe_df)
num_cols <- ncol(dataframe_df)
print(paste("Số hàng:", num_rows))
print(paste("Số cột:", num_cols))
# Hiển thị một số dòng đầu của dataframe_df
head(dataframe_df)



# Chuyển đổi cột 'published_date' thành đối tượng thời gian
dataframe_df$published_date <- ymd_hms(dataframe_df$published_date, tz = "UTC")
# Lưu trữ thời gian trong định dạng mới
dataframe_df$published_date <- format(dataframe_df$published_date, "%Y-%m-%d %H:%M:%S")
head(dataframe_df)



###### Chuyển đổi cột 'published_date' thành đối tượng thời gian
dataframe_df$published_date <- ymd_hms(dataframe_df$published_date, tz = "UTC")
# Thêm các cột mới
dataframe_df$published_time <- format(dataframe_df$published_date, "%H:%M:%S")
dataframe_df$published_year <- year(dataframe_df$published_date)
dataframe_df$published_month <- month(dataframe_df$published_date)
dataframe_df$published_day <- day(dataframe_df$published_date)
# Loại bỏ cột 'published_date' không cần thiết
dataframe_df$published_date <- NULL
head(dataframe_df)



# Lấy số hàng và số cột của dataframe_df
num_rows <- nrow(dataframe_df)
num_cols <- ncol(dataframe_df)
# Lấy tổng số phần tử trong dataframe_df
total_elements <- length(unlist(dataframe_df))
# In ra thông tin
cat("Number of rows =", num_rows, "\nNumber of columns =", num_cols, "\nSize of the dataset =", total_elements, "elements.\n")


# Mô tả tóm tắt của dataframe_df
summary(dataframe_df)


# Tính tổng của các cột 'likes', 'views', 'comment_count'
sums <- colSums(dataframe_df[, c('likes', 'views', 'comment_count')], na.rm = TRUE)
# In ra kết quả
print(sums)



# Mô tả tóm tắt của dataframe_df
summary_df <- summary(dataframe_df)

# Lấy giá trị trung bình cho các biến 'likes', 'views', 'comment_count'
AvgLikes <- mean(dataframe_df$likes, na.rm = TRUE)
AvgViews <- mean(dataframe_df$views, na.rm = TRUE)
AvgComments <- mean(dataframe_df$comment_count, na.rm = TRUE)

# In ra kết quả
cat("Average number of views on video =", AvgViews, "\nAverage number of likes on video =", AvgLikes, "\nAverage number of comment on video =", AvgComments, "\n")




########Phân phối lượt likes trên video
# Cài đặt theme tương tự seaborn
theme_set(theme_minimal(base_size = 14) + theme(axis.text = element_text(color="black")))
# Thiết lập kích thước của đồ thị
options(repr.plot.width=12, repr.plot.height=5)
# Đặt màu nền của đồ thị
par(bg = "#00000000")
# Bắt đầu vẽ biểu đồ (ví dụ: histogram cho biến 'likes')
ggplot(data = dataframe_df, aes(x = likes)) +
  geom_histogram(fill = "steelblue", color = "black", bins = 30) +
  labs(title = "Phân phối Lượt thích trên Video",
       x = "Lượt Thích",
       y = "Tần suất") +
  theme_minimal(base_size = 14) +
  theme(axis.text = element_text(color="black"))



########## biểu đồ tròn###################
# Tạo dữ liệu cho biểu đồ tròn
pie_data1 <- data.frame(labels = c('Likes', 'No Likes'),
                        values = c(sum(dataframe_df$likes), sum(dataframe_df$views) - sum(dataframe_df$likes)))

pie_data2 <- data.frame(labels = c('Likes', 'Commenters'),
                        values = c(sum(dataframe_df$likes), sum(dataframe_df$comment_count)))

pie_data3 <- data.frame(labels = c('Comments', 'Non-Commenters'),
                        values = c(sum(dataframe_df$comment_count), sum(dataframe_df$views) - sum(dataframe_df$comment_count)))

# Tạo biểu đồ tròn với màu fill khác nhau và thêm % dữ liệu
create_pie_chart <- function(pie_data, title, colors) {
  pie_chart <- ggplot(pie_data, aes(x = "", y = values, fill = labels)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    geom_text(aes(label = sprintf("%.1f%%", (values / sum(values)) * 100)),
              position = position_stack(vjust = 0.5)) +
    coord_polar("y") +
    ggtitle(title) +
    theme_void() +
    scale_fill_manual(values = colors)
  
  return(pie_chart)
}

# Tạo biểu đồ tròn với % dữ liệu
pie1 <- create_pie_chart(pie_data1, "Likes vs Dislikes", c("blue", "orange"))
pie2 <- create_pie_chart(pie_data2, "Likes vs Commenters", c("green", "purple"))
pie3 <- create_pie_chart(pie_data3, "Viewsers vs Total Comments", c("red", "yellow"))

# Sắp xếp và hiển thị biểu đồ tròn
grid.arrange(pie1, pie2, pie3, ncol = 3)


###################### Biểu đồ cột####################3
# Lọc dữ liệu cho năm 2021
data_2021 <- subset(dataframe_df, published_year == 2021)

# Tạo biểu đồ cột
bar_data <- data.frame(
  month = factor(month.abb),  # Sử dụng month.abb để có tất cả các tháng
  likes = rep(0, 12),
  views = rep(0, 12)
)

# Tính tổng likes và views cho mỗi tháng
for (i in 1:12) {
  bar_data$likes[i] <- sum(data_2021[data_2021$published_month == i, 'likes'])
  bar_data$views[i] <- sum(data_2021[data_2021$published_month == i, 'views'])
}

# Thiết lập kích thước của đồ thị
options(repr.plot.width = 10, repr.plot.height = 6)

# Vẽ biểu đồ
ggplot(bar_data, aes(x = month)) +
  geom_bar(aes(y = likes), stat = "identity", position = "dodge", fill = "blue", width = 0.4, alpha = 0.7) +
  geom_bar(aes(y = views), stat = "identity", position = "dodge", fill = "red", width = 0.4, alpha = 0.7) +
  labs(title = "Likes and Views in 2021", x = "Month", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = as.character(bar_data$month))


###################hàm cho biết mỗi tháng phát hành bao nhiêu video#####
# Tính số lượng mẫu trong mỗi nhóm (published_month)
count_per_month <- table(dataframe_df$published_month)
# Hiển thị kết quả
print(count_per_month)



###############biểu đồ scatter##########
options(warn=-1)
# Tạo hình vẽ với 3 đồ thị phân tán
par(mfrow=c(1, 3), mar=c(4, 4, 2, 2))
# Đồ thị 1: views
plot(dataframe_df$published_month, dataframe_df$views, main="Figure1", xlab="Published Month", ylab="Views", col="blue", pch=16)
axis(1, at=dataframe_df$published_month, labels=dataframe_df$published_month, las=2)
# Đồ thị 2: likes
plot(dataframe_df$published_month, dataframe_df$likes, main="Figure2", xlab="Published Month", ylab="Likes", col="red", pch=16)
axis(1, at=dataframe_df$published_month, labels=dataframe_df$published_month, las=2)
# Đồ thị 3: comment_count
plot(dataframe_df$published_month, dataframe_df$comment_count, main="Figure3", xlab="Published Month", ylab="Comment Count", col="green", pch=16)
axis(1, at=dataframe_df$published_month, labels=dataframe_df$published_month, las=2)
# Bật lại cảnh báo
options(warn=0)


#########thống kê trong các năm phát hành bao nhiêu video#########
# Sử dụng hàm table để đếm số lượng quan sát cho từng giá trị của biến published_year
count_by_year <- table(dataframe_df$published_year)
# Hiển thị kết quả
print(count_by_year)
# Sắp xếp dataframe_df theo cột 'views' giảm dần và lấy 10 hàng đầu tiên
top_10_views <- dataframe_df[order(-dataframe_df$views), ][1:10, ]
# Hiển thị kết quả
print(top_10_views)
# Sắp xếp dataframe_df theo cột 'views' tăng dần và lấy 10 hàng đầu tiên
bottom_10_views <- dataframe_df[order(dataframe_df$views), ][1:10, ]
# Hiển thị kết quả
print(bottom_10_views)


############ Chứng minh giả thuyết
#### Kiểm định t
# Giả sử dataframe_df là dataframe chứa dữ liệu của bạn
# Nếu dataframe_df chưa có, bạn cần thay thế tên dataframe thực tế
# Lọc dữ liệu cho từng năm
data_2021 <- dataframe_df[dataframe_df$published_year == 2021, "likes"]
data_2022 <- dataframe_df[dataframe_df$published_year == 2022, "likes"]
# Kiểm định t độc lập
t_test_result <- t.test(data_2021, data_2022)
# Hiển thị kết quả
print(t_test_result)

##### Phân tích ANOVA
# Giả sử dataframe_df là dataframe chứa dữ liệu của bạn
# Nếu dataframe_df chưa có, bạn cần thay thế tên dataframe thực tế
# Tạo mô hình ANOVA
anova_model <- aov(likes ~ published_year, data = dataframe_df)
# Kiểm tra kết quả
summary(anova_model)

################## Dự đoán bằng hồi quy
# Giả sử dataframe_df là dataframe chứa dữ liệu của bạn
# Nếu dataframe_df chưa có, bạn cần thay thế tên dataframe thực tế
# Tạo mô hình hồi quy tuyến tính
regression_model <- lm(likes ~ views + comment_count, data = dataframe_df)
# Hiển thị tóm tắt mô hình
summary(regression_model)
# Dự đoán lượng "likes" cho một dòng dữ liệu mới
new_data <- data.frame(views = 100000, comment_count = 50)
predicted_likes <- predict(regression_model, newdata = new_data)
# Hiển thị giá trị dự đoán
print(predicted_likes)


################## Phân tích PCA
# Giả sử dataframe_df là dataframe chứa dữ liệu của bạn
# Nếu dataframe_df chưa có, bạn cần thay thế tên dataframe thực tế
# Chọn các biến để thực hiện PCA
variables <- dataframe_df[, c("likes", "views", "comment_count")]
# Chuẩn bị dữ liệu (chuẩn hóa nếu cần)
scaled_data <- scale(variables)
# Thực hiện PCA
pca_result <- princomp(scaled_data)
# Xem tóm tắt kết quả
summary(pca_result)
# Hiển thị biểu đồ biểu diễn phần trăm phương sai được giữ lại bởi từng thành phần chính
plot(pca_result)



