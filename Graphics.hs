module Graphics ( header ) where

import PrintUtils

header :: IO ()
header = printText ["---------------------",
                    "###  #      #  #  ###",
                    "#  # #      #  # #   ",
                    "###  #      ####  ## ",
                    "#  # #      #  #    #",
                    "###  ####   #  # ### ",
                    "---------------------"]
