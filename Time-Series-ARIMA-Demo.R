##############################################
# 时间序列预模型之ARIMA模型
# Jiangwei 
# 2021年11月
##############################################

library(dplyr)
library(tidyr)
library(zoo)
library(tseries)
library(ggfortify)
library(gridExtra)
library(forecast)
library(openxlsx)

#-------------------------加载数据、数据预处理------------------------

#日期从2007年1月2日到2020年12月31日，3654原始行
df_WTI_raw <- read.xlsx(xlsxFile="F:/R_Workspace/预测石油价格/WTI日价格.xlsx", detectDates=TRUE, rows=c(1:3655)) #
#剔除NA 和2020年4月的一个异常值
df_WTI_raw = filter(df_WTI_raw,  !is.na(DCOILWTICO))
df_WTI_raw[which(df_WTI_raw$DCOILWTICO == -36.98),'DCOILWTICO'] <- 8.91 # #这一天异常，当日，WTI收报-37.63美元/桶，新闻：http://www.dyhjw.com/gold/20200506-17824.html



wti_dts<-ts(df_WTI_raw$DCOILWTICO,frequency=1,start=1)




#------------画出时序图和白噪声检验---------------------------------
autoplot(wti_dts)+ggtitle("wti_dts")
plot(wti_dts,type="l",xlab="时间",ylab="wti_dts")

Box.test(wti_dts,type ="Ljung-Box")
## p-value = 2.2e-16 说明该序列非随机数据,即不为白噪声




#------------------------平稳性检验---------------------------
ndiffs(wti_dts)#该函数返回需要几阶差分可以达到平稳序列，结果表明序列需要进行1阶差分
wti_dts_diff<-diff(wti_dts,1)
ndiffs(wti_dts_diff)#结果表明无需进行
plot(wti_dts_diff,type="l",xlab="时间",ylab="wti_dts_diff") 
#从上图可以看出x.dif序列值在0的附近波动，没有存在显著地波动起伏大的情况，基本为平稳特征.

# 对差分序列wti_dts_diff进行adf单位根检验：
adf.test(wti_dts_diff)
# 从wti_dts_diff的adf单位根检验p=0.01小于显著水平a=0.05，故拒绝原假设，所有wti_dts_diff是平稳序列.



#-----------------手动定阶----------------------------------
# 绘制差分序列的自相关图和偏自相关图，以确定模型参数；
p1 <- autoplot(stats::acf(wti_dts_diff,lag.max = 40,plot = F))+  ggtitle("wti_dts序列自相关图")
p2 <- autoplot(stats::pacf(wti_dts_diff,lag.max = 40,plot = F))+  ggtitle("wti_dts序列偏自相关图")
gridExtra::grid.arrange(p1,p2,nrow=2)
# 从上图可以看出x.dif的ACF，PACF是均显示不截尾的性质（PACF：lag12,20...; ACF：lag:6,12... 在2倍标准差外）,故认为可以尝试使用模型ARMA(1,1)


#--------------采用AICC, AIC, BIC准则，auto.arima模型自动定阶----------------
ARIMA=auto.arima(wti_dts_diff, seasonal = FALSE, stepwise=FALSE, approximation=FALSE)#自动创建最优模型
summary(ARIMA)
# ARIMA(2,1,0),也就是d=1是需要一阶差分后，序列才平稳，然后对它进行自回归模型是ARMA(2,0).即最后得到模型为wti_dts_diff序列的ARMA(2,0)模型




#--------------------拟合模型-------------------------------------
wti_dts_diff.fit <- arima(x=wti_dts_diff, order=c(4, 0, 1))
summary(wti_dts_diff.fit)
qqnorm(wti_dts_diff.fit$residual)
qqline(wti_dts_diff.fit$residual)
Box.test(wti_dts_diff.fit$residual,type ="Ljung-Box") #白噪声检验
## p-value = 0.9853,此时，模型的残差已经是白噪声数据，数据中的信息已经充分的提取出来了


# -----------------ARIMA 预测下一天的股价 -------------------------------------
library(forecast)
wti_dts_diff.fore <- forecast(wti_dts_diff.fit, h=5)
print(wti_dts_diff.fore )



plot(c(wti_dts_diff.fore$fitted[3500:3521], wti_dts_diff.fore$mean),type='b',col='red', ylab = '一阶差分值', xlab='日期', linewidth=2)
lines(wti_dts_diff.fore$fitted[3500:3521],type='b', col = "black")

#预测的第一天
wti_dts[3522] + wti_dts_diff.fore$mean[1] #恢复至股价

#依次用前一天的真实值 + 第二天预测的差分值预测未来后一天的股价
