
install.packages("writexl")


# Carregar a biblioteca necessária
library(writexl)

# Criar os vetores com os dados
cidades <- c('Chennai', 'Kitwe', 'Ipoh', 'Akure', 'Bandung', 'Juiz de Fora', 'Viçosa', 'Guantánamo', 'Isla del Carmen', 'Texcoco de Mora', 'Kottayam', 'Madurai', 'Bekasi', 'Bhopal', 'Kochi', 'South Tangerang', 'Viçosa', 'Selangor', 'Bangkok', 'Bhopal', 'Pasir Gudang', 'Addis Ababa', 'Cebu', 'Singapore', 'Xalapa', 'Denpasar', 'Dangila', 'Ho Chi Minh', 'Medellín', 'Chandpur', 'Bhopal', 'La Trinidad', 'Sarguja', 'Addis Ababa', 'Bogotá', 'Abha', 'Bogotá', 'Accra', 'Nazaré Paulista', 'Ambikapur', 'Bobo Dioulasso', 'Bucaramanga', 'Pyin Oo Lwin', 'Addis Ababa', 'Johor Bahru', 'Nasik', 'Coimbatore', 'Kumba', 'Muscat', 'Puerto Rico', 'Johor', 'Kolkata', 'Dongguan', 'Kumasi', 'Bangkok', 'Metro City', 'Bandung', 'Singapore', 'Puyo', 'Bilaspur', 'Idukki', 'Dhaka', 'Lima', 'Kumasi', 'Nagpur', 'Bogor')
latitudes <- c(13.01784, -12.8167, 4.6685, 7.25, -6.88581, -21.6889, -20.7833, 20.1383, 18.6581, 19.464, 9.5333, 9.882095, -6.1683, 23.2417, 9.8806, -6.225, -20.48, 3.1034, 14.293, 23.1819, 1.5028, 9.0784, 10.2833, 1.3122, 19.5468, -8.5919, 11.08, 10.7756, 6.2319, 22.575, 23.1556, 16.4789, 23.2, 8.8333, 19.2833, 18.1592, 4.5981, 5.55, -23.15, 23.2, 11.1833, 7.0998, 22.0333, 9.0875, 1.517, 19.9975, 11.0167, 4.6627, 23.6088, 18.4283, 1.0165, 22.4108, 22.8167, 6.6833, 13.7525, 5.1092, 6.8849, 1.3143, -1.4837, 22.09, 9.6667, 23.7, -12.05, 6.6833, 21.15, 6.5546)
longitudes <- c(80.268242, 28.2, 101.0794, 5.195, 107.61137, -43.3444, -42.8833, -75.2061, -91.8172, -98.8263, 76.6, 78.081537, -106.8078, 77.3917, 76.2897, 106.6333, -42.51, 101.6251, 99.826, 77.4219, 103.9356, 38.7592, 123.9, 103.9109, -96.9504, -115.1731, -36.58, 106.7019, -75.5681, 90.775, 77.3669, 120.5862, 83.0333, 38.7, -99.1833, 42.3979, -74.0758, -0.2, -46.3333, 83.0333, -4.2833, -73.1115, 96.4587, 38.9086, 103.6676, 73.7898, 76.9667, 9.3977, 58.4058, -66.0379, 103.0647, 88.1886, 113.8417, -1.6167, 100.4942, 105.328, 107.6106, 103.9112, -78.0026, 82.15, 10.0, 90.3667, -77.0333, -1.6167, 79.1, 106.7234)

# Criar um data frame
coordenadas_df <- data.frame(
  Cidade = cidades,
  Latitude = latitudes,
  Longitude = longitudes
)

# Definir o nome do arquivo de saída
nome_arquivo <- "coordenadas.xlsx"

# Escrever o data frame para um arquivo .xlsx
write_xlsx(coordenadas_df, path = nome_arquivo)

# Mensagem de confirmação
print(paste("Arquivo", nome_arquivo, "criado com sucesso!"))



IGNORE_WHEN_COPYING_START
IGNORE_WHEN_COPYING_END


# Carregar a biblioteca para escrever arquivos XLSX
library(writexl)

# Criar o data frame com todos os 67 pontos de dados
coordenadas_completas_df <- data.frame(
  Cidade = c("Chennai", "Kitwe", "Ipoh", "Akure", "Bandung", "Juiz de Fora", 
             "Viçosa", "Guantánamo", "Isla del Carmen", "Texcoco de Mora", 
             "Kottayam", "Madurai", "Bekasi", "Bhopal", "Kochi", "South Tangerang", 
             "Viçosa", "Selangor", "Bangkok", "Bhopal", "Pasir Gudang", 
             "Addis Ababa", "Addis Ababa", "Cebu", "Singapore", "Xalapa", "Denpasar", 
             "Dangila", "Ho Chi Minh", "Medellín", "Chandpur", "Bhopal", 
             "La Trinidad", "Sarguja", "Addis Ababa", "Bogotá", "Abha", "Bogotá", 
             "Accra", "Nazaré Paulista", "Ambikapur", "Bobo Dioulasso", "Bucaramanga", 
             "Pyin Oo Lwin", "Addis Ababa", "Johor Bahru", "Nasik", "Coimbatore", 
             "Kumba", "Muscat", "Puerto Rico", "Johor", "Kolkata", "Dongguan", 
             "Kumasi", "Bangkok", "Metro City", "Bobo Dioulasso", "Bandung", 
             "Singapore", "Puyo", "Bilaspur", "Idukki", "Dhaka", "Lima", "Kumasi", 
             "Nagpur", "Bogor"),
  Latitude = c(13.01784, -12.816667, 4.678503, 7.25, -6.88581, -21.688889, 
               -20.8, -20.138333, 18.658056, 19.464028, 9.533333, 9.882095, 
               -6.168333, 23.241667, 9.790556, -6.225, -20.48, 3.103361, 
               14.293, 23.181944, 1.502778, 8.165278, 9.078389, 10.283333, 
               1.31225, 19.546806, -8.591944, 11.08, 10.775556, 6.231944, 
               22.575, 23.155556, 16.478861, 23.2, 8.75, 19.283333, 18.1592, 
               4.598056, 5.55, -23.15, 23.2, 11.183333, 7.099861, 22.033333, 
               9.0875, 1.517139, 19.9975, 11.016667, 4.662694, 23.608828, 
               18.428278, 1.016583, 22.410833, 22.866667, 6.683333, 13.7525, 
               5.109194, 11.183333, 6.884889, 1.314294, -1.4837, 22.09, 
               9.833333, 23.7, -12.05, 6.683333, 21.15, 6.554639),
  Longitude = c(80.268242, 28.2, 101.0794, 5.195, 107.61137, -43.344444, 
                -42.883333, -75.206111, -91.827778, -98.826292, 76.6, 78.081537, 
                -106.807778, 77.391667, -76.248611, 106.633333, -42.51, 101.625111, 
                99.826, 77.421944, 103.935556, 38.054444, 38.759208, 123.9, 
                103.910861, -96.950417, -115.173056, -36.58, 106.701944, -75.568056, 
                90.775, 77.366944, 120.586167, 83.033333, 38.65, -99.183333, 
                42.397908, -74.075833, -0.2, -46.333333, 83.033333, -4.283333, 
                -73.1115, 96.458736, 38.905833, 103.667556, 73.789806, 76.966667, 
                9.397675, 58.402556, -66.037861, 103.064711, 88.188611, 113.841667, 
                -1.616667, 100.494167, 105.328, -4.283333, 107.610611, 103.911197, 
                -78.0026, 82.15, 76.940000, 90.366667, -77.033333, -1.616667, 79.1, 106.723389)
)

# Definir o nome do arquivo de saída
nome_do_arquivo <- "coordenadas_completas.xlsx"

# Escrever o data frame para um arquivo .xlsx
write_xlsx(coordenadas_completas_df, path = nome_do_arquivo)

# Imprimir uma mensagem de confirmação no console
print(paste("Arquivo '", nome_do_arquivo, "' foi criado com sucesso no seu diretório de trabalho!", sep=""))



# Criar o data frame com os dados convertidos
coordenadas_df <- data.frame(
  Cidade = c("Laguindingan", "Alubijid", "El Salvador", "Port Harcourt", "Ilorin", 
             "Niamey", "Maradi", "Aracaju", "Belo Horizonte", "Camaçari", 
             "Campos dos Goytacazes", "Duque de Caxias", "Feira de Santana", "Ilhéus", 
             "Jaboatão dos Guararapes", "Leme", "Macaé", "Maceió", "Mauá", "Osasco", 
             "Recife", "São Gonçalo", "Serra", "Simões Filho", "Suzano"),
  Latitude = c(8.583333, 8.571411, 8.566667, 4.789444, 8.499167, 13.583333, 
               13.533333, -10.9095, -19.9167, -12.6978, -21.7522, -22.7858, 
               -12.2669, -14.7889, -8.1131, -22.1856, -22.3708, -9.6658, 
               -23.6678, -23.5325, -8.0476, -22.8231, -20.1286, -12.785, -23.5431),
  Longitude = c(124.45, 124.4751, 124.5167, 6.973056, 4.500556, 2.25, 
                7.666667, -37.0748, -43.9333, -38.3242, -41.3244, -43.3097, 
                -38.9669, -39.0494, -35.015, -47.39, -41.7869, -35.7353, 
                -46.4614, -46.7917, -34.877, -43.0539, -40.3078, -38.4042, -46.3114)
)

# Definir o nome do arquivo de saída
nome_do_arquivo <- "coordenadas_convertidas.xlsx"

# Escrever o data frame para um arquivo .xlsx
write_xlsx(coordenadas_df, path = nome_do_arquivo)

# Imprimir uma mensagem de confirmação
print(paste("Arquivo '", nome_do_arquivo, "' criado com sucesso!", sep=""))


