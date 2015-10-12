require("nn")
local seq0 = nn.Sequential()
seq0:add(nn.Threshold())
return seq0