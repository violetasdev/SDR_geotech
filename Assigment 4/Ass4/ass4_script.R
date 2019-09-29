?formula
?model.frame
?model.matrix

#Create a formula, e.g. by `f <- y ~ x` after creating this what does this object mean, 
#or do? Does object `f` exist? Do `x` and `y` exist?
formula(z <- a ~ b)
class(f)
f
x
y
class(z)


x <- 1:10
y<-10:1
model.frame(z)
model.frame(f, data.frame(x,y)[1:3,])
data.frame(x,y)[1:3,]


##4
##http://www.physiol.ox.ac.uk/~raac/R.shtml
##https://www.jstor.org/stable/2346786?seq=7#metadata_info_tab_contents
model.frame(y~x+1)
model.frame(y~I(x+1))

?I
?terms.formula

#5
f <- factor(c(rep("a",3), rep("b", 3),rep("c", 4)))
f
plot(lm(y ~ x + f))




#6
model.frame(y~x+f)
?model.frame

#https://genomicsclass.github.io/book/pages/expressing_design_formula.html
#https://hopstat.wordpress.com/2014/06/26/be-careful-with-using-model-design-in-r/
#understanding the construction of linear models with the help of
#matrix, it will help to identify 
model.matrix(y~x+f)
?model.matrix

