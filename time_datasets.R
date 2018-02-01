data = import_data("/Users/dara/GDrive - WM/Animal Tag Lab Book/Data/20171019_v22b_table_test//20171019_v22b_table_test_combined.csv", legacy=F)

start = data[[7]][1]
end = data[[7]][nrow(data)]

duration = end - start

print(paste("Start:", start))
print(paste("End:", end))
print(paste("Length:", duration*24, "hrs"))