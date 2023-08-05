import numpy as np
x = np.array([])
with open( "p2data.txt" , "r") as file: 
    numbers = file.read()
numbers = np.fromstring( numbers , dtype = float  , sep = "\n")
print( np.size( numbers ))
# numbers = np.round( numbers , 2 )
data = np.reshape( numbers , newshape= (int(np.size(numbers)/4), 4 ) , order= 'F')
np.savetxt("p2data.csv" , data , delimiter= ',' , fmt = '%f')