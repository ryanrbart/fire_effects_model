($2 == "cover_fraction") {printf("%f %s\n",par,$2)}
($2 != "cover_fraction") {printf("%s %s\n",$1,$2)}
