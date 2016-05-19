# Below is a function of the general equation for plotting
OffATr <- function(r, m, mmin=1, Beta=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - mmin - Beta*r)) ));
}
# Below is the same equation as an expression for maths
fm  <- expression(0.5*(1+r)*(1-exp(-c*(m-mmin-Beta*r))),'m');
# We can use the above to differentiate fm wrt `m`
fmd <- D(fm,'m'); # can print fmd to show solution
# Below is fmd for analysis and plotting
OffATrd <- function(r, m, mmin=1, Beta=1, c=1){
    return(  0.5 * (1 + r) * (exp(-c * (m - mmin - Beta * r)) * c)  );
    # Also can simplify: return(  (c/2)*(1+r)*exp(-c*(m-mmin-Beta*r))  );
} # We'll need the above to find m^{*} and \gamma^{*} 
# To find m^{*}, can use the function below
findm <- function(low.guess,high.guess,rval,mmin=1,Beta=1,c=1){
    fm  <- function(m,r=rval){ 
        OffATrd(r=rval, mmin=mmin, Beta=Beta, m=m, c=c)*(0-m) + 
            OffATr(r=rval, mmin=mmin, Beta=Beta, m=m, c=c);
    }
    lg  <- fm(m=low.guess, r=rval);
    hg  <- fm(m=high.guess,r=rval);
    if(lg > 0){
        u <- low.guess;
        l <- high.guess;
    }else{
        u <- high.guess;
        l <- low.guess;
    }
    if((fm(l) > 0 & fm(u) > 0) | (fm(l) < 0 & fm(u) < 0)){
        return("Value of m is outside the range");
    }else{
        check  <- 1;
        mguess <- 0.5 * (l+u);
        i      <- 0;
        while(abs(check) > 0.001 & i < 10000){
            check <- fm(r=rval, m=mguess);
            if(check > 0){
                u      <- mguess;
                mguess <- 0.5*(l+mguess); 
            }else{
                l      <- mguess;
                mguess <- 0.5*(u+mguess);
            }
            i <- i+1;
        }
        return(mguess);
    }
} # Running the below returns the estimate

find.Beta <- function(lg,hg,mstari,mstaro,cvl=1,mmin=1,rval=0.5){
    Ir <- function(Beta, m=mstari, cvl=cvl, mmin=mmin, rval=rval){
        return( 0.5*(1+rval)*(1-exp(-cvl*(m-mmin-rval*Beta))) );
    }
    oD <- exp(-cvl*(mstaro-mmin-0.0));
    Of <- Ir(Beta=0, m=mstaro, cvl=cvl, mmin=mmin, rval=0);
    uu <- Ir(Beta=lg, m=mstari, cvl=cvl, mmin=mmin, rval=rval);
    ll <- Ir(Beta=hg, m=mstari, cvl=cvl, mmin=mmin, rval=rval);
    if((ll > Of & uu > Of) | (ll < Of & uu < Of)){
        return("Value of m is outside the range");
    }else{
        check  <- 1;
        mguess <- 0.5 * (lg+hg);
        i      <- 0;
        while(abs(check) > 0.001 & i < 1000){
            check <- Ir(Beta=mguess, m=mstari, cvl=cvl, mmin=mmin, rval=rval) - Of;
            if(check < 0){
                hg     <- mguess;
                mguess <- 0.5*(lg+mguess); 
            }else{
                lg     <- mguess;
                mguess <- 0.5*(hg+mguess);
            }
            i <- i+1;
        }
        return(mguess);
    }
} # Running the below returns the estimate

btest <- 1;

newm  <- findm(low.guess=0,high.guess=20, rval=0.5, Beta=btest, mmin=1, c=1); newm;
newb  <- find.Beta(lg=0,hg=20,mstari=newm,mstaro=2.15,cvl=1,mmin=1,rval=0.5); newb;
btest <- newb;



Ir(Beta=newb, m=mstaro, cvl=cvl, mmin=mmin, rval=rval);
Ir(Beta=newb, m=newm, cvl=cvl, mmin=mmin, rval=rval);


fm  <- yacas(expression(  (cval/2)*(1+rval)*exp(-cval*(mval-mmin-Beta*rval))*(0-mval) +
                    (1/2)*(1+rval)*(1-exp(-cval*(mval-mmin-Beta*rval)))
                    ,'m');


fm  <- yacas(expression( (cval/2)*(1+rval)*exp(-cval*(mval-mmin-Beta*rval))*(0-mval) +
                        (1/2)*(1+rval)*(1-exp(-cval*(mval-mmin-Beta*rval)))));


((1-Exp(-cval*(mval-mmin-Beta*rval)))*(rval+1))/2 - 
    (mval*Exp(-cval*(mval-mmin-Beta*rval))*(rval+1)*cval)/2



yacas("Solve(x/(1+x) == a, x)")


yacas("Solve(
      0 == (0-m)*c*Exp(-c*(m-z-b*r))+1-Exp(-c*(m-z-b*r))
      ,r)")


fr  <- expression(0.5*(1+r)*(1-exp(-c*(m-mmin-Beta*r))),'r');
# We can use the above to differentiate fm wrt `m`
frd <- D(fr,'r'); # can print fmd to show solution

#######################################################################
# XXX JUST MUSCLE IT...
#######################################################################


# Below is a function of the general equation for plotting
OffATr <- function(r, m, mmin=1, Beta=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - mmin - Beta*r)) ));
}
# Below is the same equation as an expression for maths
fm  <- expression(0.5*(1+r)*(1-exp(-c*(m-mmin-Beta*r))),'m');
# We can use the above to differentiate fm wrt `m`
fmd <- D(fm,'m'); # can print fmd to show solution
# Below is fmd for analysis and plotting
OffATrd <- function(r, m, mmin=1, Beta=1, c=1){
    return(  0.5 * (1 + r) * (exp(-c * (m - mmin - Beta * r)) * c)  );
    # Also can simplify: return(  (c/2)*(1+r)*exp(-c*(m-mmin-Beta*r))  );
} # We'll need the above to find m^{*} and \gamma^{*} 
# To find m^{*}, can use the function below
findm <- function(low.guess,high.guess,rval,mmin=1,Beta=1,c=1){
    fm  <- function(m,r=rval){ 
        OffATrd(r=rval, mmin=mmin, Beta=Beta, m=m, c=c)*(0-m) + 
            OffATr(r=rval, mmin=mmin, Beta=Beta, m=m, c=c);
    }
    lg  <- fm(m=low.guess, r=rval);
    hg  <- fm(m=high.guess,r=rval);
    if(lg > 0){
        u <- low.guess;
        l <- high.guess;
    }else{
        u <- high.guess;
        l <- low.guess;
    }
    if((fm(l) > 0 & fm(u) > 0) | (fm(l) < 0 & fm(u) < 0)){
        return("Value of m is outside the range");
    }else{
        check  <- 1;
        mguess <- 0.5 * (l+u);
        i      <- 0;
        while(abs(check) > 0.000001 & i < 1000000){
            check <- fm(r=rval, m=mguess);
            if(check > 0){
                u      <- mguess;
                mguess <- 0.5*(l+mguess); 
            }else{
                l      <- mguess;
                mguess <- 0.5*(u+mguess);
            }
            i <- i+1;
        }
        return(mguess);
    }
} # Running the below returns the estimate
# Below returns m^{*} for outbreeders and inbreeders, respectively
r00m <- findm(low.guess=0,high.guess=4,rval=0.0,mmin=1,Beta=0.2,c=1);
r05m <- findm(low.guess=0,high.guess=4,rval=0.5,mmin=1,Beta=0.2,c=1);
# Below finds the tangent slope, \gamma^{*}
r00g <- OffATr(r=0,   m=r00m, mmin=1, Beta=1, c=1) / r00m;
r05g <- OffATr(r=0.5, m=r05m, mmin=1, Beta=1, c=1) / r05m;
# The code below plots everything
PI <- seq(from=0,to=4,by=0.01);
Alleles_IBD_outbr <- OffATr(r=0.0, m=PI, mmin=1, Beta=1, c=1);
Alleles_IBD_inbr  <- OffATr(r=0.5, m=PI, mmin=1, Beta=1, c=1);
par(mar=c(5,5,2,2));
plot(PI,Alleles_IBD_outbr,type="l",lwd=3,ylim=c(0,1),
     xlab=expression(paste("Parental investment (",italic(m),")")),
     ylab=expression(paste("IBD alleles in offspring (",zeta[off],")")),
     cex.lab=1.5,cex.axis=1.5);
abline(h=0,lty="dotted",lwd=0.8);
points(PI,Alleles_IBD_inbr,type="l",lwd=3,lty="dashed");
abline(a=0,b=r00g,lty="solid", lwd=1); # Tangent line r = 0
abline(a=0,b=r05g,lty="dashed",lwd=1); # Tangent line




rr    <- seq(from=0,to=1,by=0.01);
bb    <- seq(from=0,to=5,by=0.05);
mopts <- matrix(data=0,nrow=length(rr),ncol=length(bb));
for(j in 1:length(bb)){
  for(i in 1:length(rr)){
    mopts[i,j] <- findm(low.guess=0,high.guess=10,rval=rr[i],mmin=1,Beta=bb[j],c=4);
  }
  print(j);
}
image(z=mopts,xaxt="n",yaxt="n",col=gray.colors(n=1000,start=1,end=0.0))
axis(side=1,at=seq(from=0,to=1,by=0.2),labels=5*seq(from=0,to=1,by=0.2))
axis(side=2,at=seq(from=0,to=1,by=0.2),labels=1*seq(from=0,to=1,by=0.2))




persp(x=rr,y=bb,z=mopts,ylab=expression("Inbreeding depression"),
      xlab=expression("Coefficient of relatedness (r)"),
      zlab=expression("Optimum parental investment (m*)"));


tt <- function(m){
 (1/2)*(1+0)*exp(-1*(m-1-0))*(0-m) + (1/2)*(1+0)*(1-exp(-1*(m-1-0)));   
}


tt <- function(m,r,b){
    (1/2)*(1+r)*exp(-1*(m-1-r*b))*(0-m) + (1/2)*(1+r)*(1-exp(-1*(m-1-r*b)));   
}
mm <- seq(from=0,to=5,by=0.01);
t1 <- tt(m=mm,r=0,b=1);
t2 <- tt(m=mm,r=0.25,b=1);
t3 <- tt(m=mm,r=0.5,b=1);




plot(mm,t1,type="l",lwd=3)
points(mm,t2,type="l",lwd=3,col="red")
points(mm,t3,type="l",lwd=3,col="blue")






abline(h=0)




oo <- function(m,r,b,c,mmin){
    (1 - exp(-c*(m-mmin-b*r)));   
}
mm <- seq(from=0,to=5,by=0.01);
t1 <- oo(m=mm,r=0,b=1,c=1,mmin=1);
t2 <- oo(m=mm,r=0.25,b=1,c=1,mmin=1);
t3 <- oo(m=mm,r=0.5,b=1,c=1,mmin=1);

plot(mm,t1,type="l",lwd=3,ylim=c(0,1))
points(mm,t2,type="l",lwd=3,col="red")
points(mm,t3,type="l",lwd=3,col="blue")


# Below is the same equation as an expression for maths
fmm  <- expression((1-exp(-c*(m-r))),'m');
# We can use the above to differentiate fm wrt `m`
fmmd <- D(fmm,'m'); # can print fmd to show solution





ch1 <- function(m,c=1,r=0) 1 + m*c
ch2 <- function(m,c=1,r=0) exp(c*(m-1-1*r))
mmm <- seq(from=0,to=5,by=0.01);

plot(x=mmm,y=ch1(mmm),type="l",lwd=2,col="black");
points(x=mmm,y=ch2(mmm),type="l",lwd=2,col="red");


dd <- function(m,c=1,r=0) c*exp(-c*(m-r));
plot(x=mmm,y=dd(mmm,c=1,r=0),type="l",lwd=2,col="black");
points(x=mmm,y=dd(mmm,c=1,r=0.1),type="l",lwd=2,col="red");
points(x=mmm,y=dd(mmm,c=1,r=0.2),type="l",lwd=2,col="blue");




mmm <- seq(from=0,to=5,by=0.01);

plot(x=mmm,y=ch1(mmm),type="l",lwd=2,col="black");
points(x=mmm,y=ch2(mmm),type="l",lwd=2,col="red");




mmm <- seq(from=0,to=5,by=0.01);
rr <- seq(from=0,to=1,by=0.01);
im <- function(c,m,r) (1/c)*(exp(c*(m-1-r))-1);
yi <- im(c=1,m=mmm,r=0);

plot(x=mmm,y=yi,type="l",lwd=2,col="black",ylim=c(0,5));
points(x=mmm,y=im(c=1,m=mmm,r=0.5),type="l",lwd=2,col="red");
abline(a=0,b=1)





points(x=rr,y=ch2(mmm),type="l",lwd=2,col="red");


frr <- function(m,c) (1/c)*log(m*c + 1);
frr(m=seq(from=0,to=1,by=0.01),c=1)

# Below is the same equation as an expression for maths
rff  <- expression((1/b)*(m-z+(1/c)*log(1/(m*c+1))),'m');
# We can use the above to differentiate fm wrt `m`
fm1 <- D(rff,'m'); # can print fmd to show solution






rff  <- expression((1/beta)*(mopt-mmin+(1/c)*log(1/(m*c+1))),'m');
# We can use the above to differentiate fm wrt `m`
fm1 <- D(rff,'m'); # can print fmd to show solution







rrmm  <- expression((1/beta)*(mopt-mmin+(1/c)*log(1/(mopt*c+1))),'mopt');
# We can use the above to differentiate fm wrt `m`
drdm <- D(rrmm,'mopt'); # can print fmd to show solution
# Note that drdm simplifies considerably from what R produces



OffATr <- function(r, m, mmin=1, Beta=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - mmin - Beta*r)) ));
}
mm <- seq(from=0,to=4,by=0.01);
yy <- OffATr(r=0,m=mm,c=1);
y2 <- OffATr(r=0,m=mm,c=-1);

plot(x=mm,y=yy,type="l",lwd=3);
points(x=mm,y=y2,type="l",lwd=3,col="red");


















# Below is a function of the general equation for plotting
OffATr <- function(r, m, mmin=1, Beta=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - mmin - Beta*r)) ));
}
# Below is the same equation as an expression for maths
fm  <- expression(0.5*(1+r)*(1-exp(-c*(m-mmin-Beta*r))),'m');
# We can use the above to differentiate fm wrt `m`
fmd <- D(fm,'m'); # can print fmd to show solution
# Below is fmd for analysis and plotting
OffATrd <- function(r, m, mmin=1, Beta=1, c=1){
    return(  0.5 * (1 + r) * (exp(-c * (m - mmin - Beta * r)) * c)  );
    # Also can simplify: return(  (c/2)*(1+r)*exp(-c*(m-mmin-Beta*r))  );
} # We'll need the above to find m^{*} and \gamma^{*} 
# To find m^{*}, can use the function below
findm <- function(low.guess,high.guess,rval,mmin=1,Beta=1,c=1){
    fm  <- function(m,r=rval){ 
        OffATrd(r=rval, mmin=mmin, Beta=Beta, m=m, c=c)*(0-m) + 
            OffATr(r=rval, mmin=mmin, Beta=Beta, m=m, c=c);
    }
    lg  <- fm(m=low.guess, r=rval);
    hg  <- fm(m=high.guess,r=rval);
    if(lg > 0){
        u <- low.guess;
        l <- high.guess;
    }else{
        u <- high.guess;
        l <- low.guess;
    }
    if((fm(l) > 0 & fm(u) > 0) | (fm(l) < 0 & fm(u) < 0)){
        return("Value of m is outside the range");
    }else{
        check  <- 1;
        mguess <- 0.5 * (l+u);
        i      <- 0;
        while(abs(check) > 0.000001 & i < 1000000){
            check <- fm(r=rval, m=mguess);
            if(check > 0){
                u      <- mguess;
                mguess <- 0.5*(l+mguess); 
            }else{
                l      <- mguess;
                mguess <- 0.5*(u+mguess);
            }
            i <- i+1;
        }
        return(mguess);
    }
} # Running the below returns the estimate
# Below returns m^{*} for outbreeders and inbreeders, respectively
r00m <- findm(low.guess=0,high.guess=4,rval=0.0,mmin=1,Beta=1,c=1);
r05m <- findm(low.guess=0,high.guess=4,rval=0.5,mmin=1,Beta=1,c=1);
# Below finds the tangent slope, \gamma^{*}
r00g <- OffATr(r=0,   m=r00m, mmin=1, Beta=1, c=1) / r00m;
r05g <- OffATr(r=0.5, m=r05m, mmin=1, Beta=1, c=1) / r05m;
# The code below plots everything
PI <- seq(from=0,to=4,by=0.01);
Alleles_IBD_outbr <- OffATr(r=0.0, m=PI, mmin=1, Beta=1, c=1);
Alleles_IBD_inbr  <- OffATr(r=0.5, m=PI, mmin=1, Beta=1, c=1);
par(mar=c(5,5,2,2));
plot(PI,Alleles_IBD_outbr,type="l",lwd=3,ylim=c(0,1),
     xlab=expression(paste("Parental investment (",italic(m),")")),
     ylab=expression(paste("IBD alleles in offspring (",zeta[off],")")),
     cex.lab=1.5,cex.axis=1.5);
abline(h=0,lty="dotted",lwd=0.8);
points(PI,Alleles_IBD_inbr,type="l",lwd=3,lty="dashed");
abline(a=0,b=r00g,lty="solid", lwd=1); # Tangent line r = 0
abline(a=0,b=r05g,lty="dashed",lwd=1); # Tangent line r = 0.5
#abline(v=r00m,lty="solid", lwd=0.5); # Position of m^{*} for r = 0
#abline(v=r05m,lty="dashed",lwd=0.5); # Position of m^{*} for r = 0.5



bss <- seq(from=0.5,to=10,by=0.5);
gamz <- function(rvl,betas,mmin=1,c=1){
    gam <- rep(0,length(betas));
    for(i in 1:length(betas)){
        mvl    <- findm(low.guess=0,high.guess=10,rval=rvl,mmin=1,Beta=betas[i],c=1);
        gam[i] <- OffATr(r=rvl, m=mvl, mmin=1, Beta=betas[i], c=1)
    }
    return(gam);
}
rg000 <- gamz(rvl=0.000,betas=bss);
rg125 <- gamz(rvl=0.125,betas=bss);
rg250 <- gamz(rvl=0.250,betas=bss);
rg500 <- gamz(rvl=0.500,betas=bss);
par(mar=c(5,5,1,1));
plot(x=bss,y=rg000,type="b",lwd=3,ylim=c(0,1),pch=1,
     xlab=expression(paste("Inbreeding depression (",beta,")")),
     ylab=expression(paste("Rate of fitness increase (",gamma,"*)")),
     cex.lab=1.5,cex.axis=1.5);
points(x=bss,y=rg125,type="b",lwd=3,pch=2);
points(x=bss,y=rg250,type="b",lwd=3,pch=3);
points(x=bss,y=rg500,type="b",lwd=3,pch=4);
abline(h=0,lty="dotted",lwd=0.8);


bss <- seq(from=0.5,to=10,by=0.25);
gamz <- function(rvl,betas,mmin=1,c=1){
    mvl <- rep(0,length(betas));
    gam <- rep(0,length(betas));
    for(i in 1:length(bss)){
        mvl[i] <- findm(low.guess=0,high.guess=10,rval=rvl,mmin=1,Beta=betas[i],c=1);
        gam[i] <- OffATr(r=rvl, m=mvl[i], mmin=1, Beta=betas[i], c=1) / mvl[i];
    }
    return(list(mvl=mvl,gam=gam));
}










bss <- seq(from=0.5,to=5,by=0.25);
gamz <- function(rvl,betas,mmin=1,c=1){
    mvl <- rep(0,length(betas));
    gam <- rep(0,length(betas));
    for(i in 1:length(bss)){
        mvl[i] <- findm(low.guess=0,high.guess=10,rval=rvl,mmin=1,Beta=betas[i],c=1);
        gam[i] <- OffATr(r=rvl, m=mvl[i], mmin=1, Beta=betas[i], c=1) / mvl[i];
    }
    return(list(mvl=mvl,gam=gam));
}
rg000 <- gamz(rvl=0.000,betas=bss);
rg125 <- gamz(rvl=0.125,betas=bss);
rg250 <- gamz(rvl=0.250,betas=bss);
rg500 <- gamz(rvl=0.500,betas=bss);
#par(mfrow=c(2,1),mar=c(0.25,5,1,1),lwd=2);
layout(matrix(data=c(1,2), nrow=2, ncol=1, byrow = TRUE),
       widths=c(1,1), heights=c(1,1))
#----------------------------------------------------------------
par(mar=c(1,5,2,1),lwd=2);
plot(x=bss,y=rg000$mvl,type="l",lwd=2,ylim=c(2,6),pch=1.5,xaxt="n",
     xlab="",
     ylab=expression(paste("Optimum parental investment (",italic(m),"*)")),
     cex.lab=1.5,cex.axis=1.5);
points(x=bss,y=rg125$mvl,type="l",lwd=2);
points(x=bss,y=rg250$mvl,type="l",lwd=2);
points(x=bss,y=rg500$mvl,type="l",lwd=2);
text(x=bss[17],y=rg000$mvl[17]+0.15,labels="r=0",srt=-0,cex=1.25);
text(x=bss[17],y=rg125$mvl[17]+0.15,labels="r=1/8",srt=6,cex=1.25);
text(x=bss[17],y=rg250$mvl[17]+0.15,labels="r=1/4",srt=12,cex=1.25);
text(x=bss[17],y=rg500$mvl[17]+0.15,labels="r=1/2",srt=18,cex=1.25);
text(x=4.95,y=5.9,labels="A",cex=2.5);
#----------------------------------------------------------------
par(mar=c(5,5,0.25,1),lwd=2);
plot(x=bss,y=rg000$gam,type="l",lwd=2,ylim=c(0.12,0.22),pch=1.5,yaxt="n",
     xlab=expression(paste("Inbreeding depression (",beta,")")),
     ylab=expression(paste("Rate of fitness increase (",gamma,"*)")),
     cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.12,0.16,0.20),cex.axis=1.5);
points(x=bss,y=rg125$gam,type="l",lwd=2);
points(x=bss,y=rg250$gam,type="l",lwd=2);
points(x=bss,y=rg500$gam,type="l",lwd=2);
text(x=bss[17],y=rg000$gam[17]+0.003,labels="r=0",srt=-0,cex=1.25);
text(x=bss[17],y=rg125$gam[17]+0.003,labels="r=1/8",srt=-6,cex=1.25);
text(x=bss[17],y=rg250$gam[17]+0.003,labels="r=1/4",srt=-12,cex=1.25);
text(x=bss[17],y=rg500$gam[17]+0.003,labels="r=1/2",srt=-18,cex=1.25);
text(x=4.95,y=0.215,labels="B",cex=2.5);


rgamma00 <- 0.1589222;
bss <- seq(from=2.33525,to=2.33527,by=0.000001)
rgamma05 <- gamz(rvl=0.500,betas=bss); rgamma05;


b5 <- 2.335264
m5 <- 3.719273
g5 <- 0.1589222

t1 <- exp(-1*(2.146194-1-2.335264*0))
t2 <- exp(-1*(3.719273-1-2.335264*0.5))

OffATr <- function(r, m, mmin=1, Beta=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - mmin - Beta*r)) ));
}


0.5*(1-exp(-1*(2.146194 - 1 - 1*0)))

exp(-1*(2.85 - 1 - 1*0.5))




beteq <- 2.335264; # eyeballed then found this value
m00eq <- 2.146187; # Iteratively found these values
m05eq <- 3.719273;
r00eq <- OffATr(r=0,  m=m00eq,Beta=beteq,mmin=1,c=1);
r05eq <- OffATr(r=0.5,m=m05eq,Beta=beteq,mmin=1,c=1);
g00eq <- r00eq / m00eq;
g05eq <- r05eq / m05eq;






(1/2)*(1+0)*(1-exp(-1*(2.146 - 1 - 1*0)));


(2.146/3.719)*(1/2)*(1+.5)*(1-exp(-1*(3.719 - 1 - 1*0.5)));















#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

setwd("~/Dropbox/DuthieManu/Inbreeding_Care");

# Below is a function of the general equation for plotting
OffATr <- function(r, m, mmin=1, Beta=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - mmin - Beta*r)) ));
}
# Below is the same equation as an expression for maths
fm  <- expression(0.5*(1+r)*(1-exp(-c*(m-mmin-Beta*r))),'m');
# We can use the above to differentiate fm wrt `m`
fmd <- D(fm,'m'); # can print fmd to show solution
# Below is fmd for analysis and plotting
OffATrd <- function(r, m, mmin=1, Beta=1, c=1){
    return(  0.5 * (1 + r) * (exp(-c * (m - mmin - Beta * r)) * c)  );
    # Also can simplify: return(  (c/2)*(1+r)*exp(-c*(m-mmin-Beta*r))  );
} # We'll need the above to find m^{*} and \gamma^{*} 
# To find m^{*}, can use the function below
findm <- function(low.guess,high.guess,rval,mmin=1,Beta=1,c=1){
    fm  <- function(m,r=rval){ 
        OffATrd(r=rval, mmin=mmin, Beta=Beta, m=m, c=c)*(0-m) + 
            OffATr(r=rval, mmin=mmin, Beta=Beta, m=m, c=c);
    }
    lg  <- fm(m=low.guess, r=rval);
    hg  <- fm(m=high.guess,r=rval);
    if(lg > 0){
        u <- low.guess;
        l <- high.guess;
    }else{
        u <- high.guess;
        l <- low.guess;
    }
    if((fm(l) > 0 & fm(u) > 0) | (fm(l) < 0 & fm(u) < 0)){
        return("Value of m is outside the range");
    }else{
        check  <- 1;
        mguess <- 0.5 * (l+u);
        i      <- 0;
        while(abs(check) > 0.000001 & i < 1000000){
            check <- fm(r=rval, m=mguess);
            if(check > 0){
                u      <- mguess;
                mguess <- 0.5*(l+mguess); 
            }else{
                l      <- mguess;
                mguess <- 0.5*(u+mguess);
            }
            i <- i+1;
        }
        return(mguess);
    }
} # Running the below returns the estimate
# Below returns m^{*} for outbreeders and inbreeders, respectively
r00m <- findm(low.guess=0,high.guess=4,rval=0.0,mmin=1,Beta=1,c=1);
r05m <- findm(low.guess=0,high.guess=4,rval=0.5,mmin=1,Beta=1,c=1);
# Below finds the tangent slope, \gamma^{*}
r00g <- OffATr(r=0,   m=r00m, mmin=1, Beta=1, c=1) / r00m;
r05g <- OffATr(r=0.5, m=r05m, mmin=1, Beta=1, c=1) / r05m;
# The code below plots everything
PI <- seq(from=0,to=4,by=0.01);
Alleles_IBD_outbr <- OffATr(r=0.0, m=PI, mmin=1, Beta=1, c=1);
Alleles_IBD_inbr  <- OffATr(r=0.5, m=PI, mmin=1, Beta=1, c=1);
par(mar=c(5,5,2,2));
plot(PI,Alleles_IBD_outbr,type="l",lwd=3,ylim=c(0,1),
     xlab=expression(paste("Parental investment (",italic(m),")")),
     ylab=expression(paste("IBD alleles in offspring (",zeta[off],")")),
     cex.lab=1.5,cex.axis=1.5);
abline(h=0,lty="dotted",lwd=0.8);
points(PI,Alleles_IBD_inbr,type="l",lwd=3,lty="dashed");
abline(a=0,b=r00g,lty="solid", lwd=1); # Tangent line r = 0
abline(a=0,b=r05g,lty="dashed",lwd=1); # Tangent line r = 0.5
#abline(v=r00m,lty="solid", lwd=0.5); # Position of m^{*} for r = 0
#abline(v=r05m,lty="dashed",lwd=0.5); # Position of m^{*} for r = 0.5

OffAT <- function(r, m, B0=1, B1=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - B0 - B1*r)) ));
}

r00m.b3 <- findm(low.guess=0,high.guess=6,rval=0.0,mmin=1,Beta=3,c=1);
r05m.b3 <- findm(low.guess=0,high.guess=6,rval=0.5,mmin=1,Beta=3,c=1);
r00g.b3 <- OffATr(r=0,   m=r00m.b3, mmin=1, Beta=3, c=1) / r00m.b3;
r05g.b3 <- OffATr(r=0.5, m=r05m.b3, mmin=1, Beta=3, c=1) / r05m.b3;


bss <- seq(from=0.5,to=5,by=0.25);
gamz <- function(rvl,betas,mmin=1,c=1){
    mvl <- rep(0,length(betas));
    gam <- rep(0,length(betas));
    for(i in 1:length(bss)){
        mvl[i] <- findm(low.guess=0,high.guess=10,rval=rvl,mmin=1,Beta=betas[i],c=1);
        gam[i] <- OffATr(r=rvl, m=mvl[i], mmin=1, Beta=betas[i], c=1) / mvl[i];
    }
    return(list(mvl=mvl,gam=gam));
}
rg000 <- gamz(rvl=0.000,betas=bss);
rg125 <- gamz(rvl=0.125,betas=bss);
rg250 <- gamz(rvl=0.250,betas=bss);
rg500 <- gamz(rvl=0.500,betas=bss);
par(mfrow=c(2,1),mar=c(0.25,5,1,1),lwd=2);
layout(matrix(data=c(1,2), nrow=2, ncol=1, byrow = TRUE),
       widths=c(1,1), heights=c(1,1))
#----------------------------------------------------------------
setEPS();
postscript("BetaWithMstar.eps");
par(mar=c(5,5,0.25,1),lwd=2);
plot(x=bss,y=rg000$mvl,type="l",lwd=2,ylim=c(2,6),pch=1.5,
     xlab=expression(paste("Inbreeding depression (",beta,")")),
     ylab=expression(paste("Optimum parental investment (",italic(m),"*)")),
     cex.lab=1.5,cex.axis=1.5);
points(x=bss,y=rg125$mvl,type="l",lwd=2);
points(x=bss,y=rg250$mvl,type="l",lwd=2);
points(x=bss,y=rg500$mvl,type="l",lwd=2);
text(x=bss[17],y=rg000$mvl[17]+0.1,labels="r=0",srt=-0,cex=1.25);
text(x=bss[17],y=rg125$mvl[17]+0.1,labels="r=1/8",srt=10,cex=1.25);
text(x=bss[17],y=rg250$mvl[17]+0.1,labels="r=1/4",srt=20,cex=1.25);
text(x=bss[17],y=rg500$mvl[17]+0.12,labels="r=1/2",srt=32,cex=1.25);
#text(x=4.95,y=5.9,labels="A",cex=2.5);
dev.off();
#----------------------------------------------------------------
setEPS();
postscript("BetaWithGammastar");
par(mar=c(5,5,0.25,1),lwd=2);
plot(x=bss,y=rg000$gam,type="l",lwd=2,ylim=c(0.12,0.22),pch=1.5,yaxt="n",
     xlab=expression(paste("Inbreeding depression (",beta,")")),
     ylab=expression(paste("Rate of fitness increase (",gamma,"*)")),
     cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.12,0.16,0.20),cex.axis=1.5);
points(x=bss,y=rg125$gam,type="l",lwd=2);
points(x=bss,y=rg250$gam,type="l",lwd=2);
points(x=bss,y=rg500$gam,type="l",lwd=2);
text(x=bss[17],y=rg000$gam[17]+0.003,labels="r=0",srt=-0,cex=1.25);
text(x=bss[17],y=rg125$gam[17]+0.003,labels="r=1/8",srt=-10,cex=1.25);
text(x=bss[17],y=rg250$gam[17]+0.003,labels="r=1/4",srt=-20,cex=1.25);
text(x=bss[17],y=rg500$gam[17]+0.003,labels="r=1/2",srt=-32,cex=1.25);
#text(x=4.95,y=0.215,labels="B",cex=2.5);
dev.off();





fmn  <- expression( (0.5 * (1 + r) * (1 - exp(-c * (m - B0 - B1 * r)))) / m, 'm'); 


dfmn <- D(fmn,'m')

pdfmn <- function(m,r=0,B0=1,B1=0,c=1){
    ans <- (0.5 * (1 + r) * (exp(-c * (m - B0 - B1 * r)) * c)) /  
        (0.5 * (1 + r) * (1 - exp(-c * (m - B0 - B1 * r))));
    return(ans);
}


yacas("Solve(
      0 == -(0.5 * (1 + r) * (Exp(-c * (m - B0 - B1 * r)) * c * c)/(0.5 * 
    (1 + r) * (1 - Exp(-c * (m - B0 - B1 * r)))) + (0.5 * (1 + 
    r) * (Exp(-c * (m - B0 - B1 * r)) * c)) * (0.5 * (1 + r) * 
    (Exp(-c * (m - B0 - B1 * r)) * c))/(0.5 * (1 + r) * (1 - 
    Exp(-c * (m - B0 - B1 * r))))^2),m)")


yacas("Simplify(
      0 == -(0.5 * (1 + r) * (Exp(-c * (m - B0 - B1 * r)) * c * c)/(0.5 * 
    (1 + r) * (1 - Exp(-c * (m - B0 - B1 * r)))) + (0.5 * (1 + 
    r) * (Exp(-c * (m - B0 - B1 * r)) * c)) * (0.5 * (1 + r) * 
    (Exp(-c * (m - B0 - B1 * r)) * c))/(0.5 * (1 + r) * (1 - 
    Exp(-c * (m - B0 - B1 * r))))^2),m)")


yacas("Solve(
      0== -((0.5*(r+1)*Exp(-c*(m-B0-B1*r))*c^2)/(0.5*(r+1)*(1-Exp(-c*(m-B0-B1*r))))+
          (0.5*(r+1)*Exp(-c*(m-B0-B1*r))*c)^2/(0.5*(r+1)*(1-Exp(-c*(m-B0-B1*r))))^2),m)")


{m==B0+B1*r-Ln((Sqrt((0.125*r^3*c^2+0.375*r^2*c^2+0.375*r*c^2+0.125*c^2)^2)-
                    (0.125*r^3*c^2+0.375*r^2*c^2+0.375*r*c^2+0.125*c^2))/
                   (2*((-0.125)*r^3*c^2-0.375*r^2*c^2-0.375*r*c^2-0.125*c^2)))/c,

m==B0+B1*r-Ln((-(0.125*r^3*c^2+0.375*r^2*c^2+0.375*r*c^2+0.125*c^2+
                     Sqrt((0.125*r^3*c^2+0.375*r^2*c^2+0.375*r*c^2+0.125*c^2)^2)))/
                  (2*((-0.125)*r^3*c^2-0.375*r^2*c^2-0.375*r*c^2-0.125*c^2)))/c};


yacas("Simplify(m==B0+B1*r-Ln((Sqrt((0.125*r^3*c^2+0.375*r^2*c^2+0.375*r*c^2+0.125*c^2)^2)-
                    (0.125*r^3*c^2+0.375*r^2*c^2+0.375*r*c^2+0.125*c^2))/
                   (2*((-0.125)*r^3*c^2-0.375*r^2*c^2-0.375*r*c^2-0.125*c^2)))/c")



dfmp <- OffATrd(r=0,m=PI,mmin=1,Beta=0,c=1)
fmp  <- OffATr(r=0,m=PI,mmin=1,Beta=0,c=1)

plot(x=PI,y=fmp/PI,ylim=c(0,1),type="l",lwd=2)
points(x=PI,y=fmp,type="l",lwd=2,col="red")
abline(a=0,b=0.3410768/2.146187,col="red")
abline(h=max(fmp/PI))
abline(v=2.146187,lty="dotted")

points(x=PI,y=(dfmp/fmp)/PI,type="l",lwd=2,col="blue")




dslope <- function(r,c,m,B0,B1){
 return(0.5 * (1 + r) * (exp(-c * (m - B0 - B1 * r)) * c)/m - 
    (0.5 * (1 + r) * (1 - exp(-c * (m - B0 - B1 * r))))/m^2)
}

yacas("PrettyForm(0==(0.5*(r+1)*Exp(-c*(m-B0-B1*r))*c)/m-(0.5*(r+1)*(1-Exp(-c*(m-B0-B1*r))))/m^2)")


yy <- dslope(r=0,c=1,m=PI,B0=1,B1=1)


mopt <- 2*OffATr(m=PI,r=0.5,mmin=1,Beta=1,c=1)/OffATrd(m=PI,r=0.5,mmin=1,Beta=1,c=1);

top <- 2*OffATr(m=PI,r=0,mmin=1,Beta=0,c=1)
bot <- OffATrd(m=PI,r=0,mmin=1,Beta=0,c=1);


which(abs(test-PI)==min(abs(test-PI)))



findmp <- function(low.guess,high.guess,rval,mmin=1,Beta=1,c=1){
    fm  <- function(m,r=rval){ 
         (2*OffATr(r=rval, mmin=mmin, Beta=Beta, m=m, c=c) / 
            OffATrd(r=rval, mmin=mmin, Beta=Beta, m=m, c=c));
    }
    lg  <- fm(m=low.guess, r=rval);
    if(lg < 0){
        fixlg     <- seq(from=low.guess,to=high.guess,by=0.01);
        move      <- sum(fm(m=fixlg,r=rval) < 0);
        newg      <- fixlg[move+1];
        lg        <- fm(m=newg, r=rval);
    }
    hg  <- fm(m=high.guess,r=rval);
    if(lg > 0){
        u <- low.guess;
        l <- high.guess;
    }else{
        u <- high.guess;
        l <- low.guess;
    }
    if((fm(l) > 0 & fm(u) > 0) | (fm(l) < 0 & fm(u) < 0)){
        return("Value of m is outside the range");
    }else{
        check  <- 1;
        mguess <- 0.5 * (l+u);
        i      <- 0;
        while(abs(check) > 0.000001 & i < 1000000){
            check <- fm(r=rval, m=mguess) - mguess;
            if(check > 0){
                u      <- mguess;
                mguess <- 0.5*(l+mguess); 
            }else{
                l      <- mguess;
                mguess <- 0.5*(u+mguess);
            }
            i <- i+1;
        }
        return(mguess);
    }
} # Running the below returns the estimate





rs <- seq(from=0,to=1,by=0.1);
mm <- rep(0,length(rs));
mp <- rep(0,length(rs));
for(i in 1:length(rs)){
   mm[i] <- findm(low.guess=1,high.guess=4,rval=rs[i]);
   mp[i] <- findmp(low.guess=0,high.guess=4,rval=rs[i]);
}













bss <- seq(from=0.5,to=5,by=0.25);
gamznokin <- function(rvl,betas,mmin=1,c=1){
    mvl <- rep(0,length(betas));
    gam <- rep(0,length(betas));
    for(i in 1:length(bss)){
        mvl[i] <- findm(low.guess=0,high.guess=10,rval=0,mmin=1,Beta=betas[i],c=1);
        gam[i] <- OffATr(r=rvl, m=mvl[i], mmin=1, Beta=betas[i], c=1) / mvl[i];
    }
    return(list(mvl=mvl,gam=gam));
}
rg000nk <- gamznokin(rvl=0.000,betas=bss);
rg125nk <- gamznokin(rvl=0.125,betas=bss);
rg250nk <- gamznokin(rvl=0.250,betas=bss);
rg500nk <- gamznokin(rvl=0.500,betas=bss);
par(mar=c(5,5,0.25,1),lwd=2);
plot(x=bss,y=rg000nk$gam,type="l",lwd=2,ylim=c(0.12,0.22),pch=1.5,yaxt="n",
     xlab=expression(paste("Inbreeding depression (",beta,")")),
     ylab=expression(paste("Rate of fitness increase (",gamma,"*)")),
     cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.12,0.16,0.20),cex.axis=1.5);
points(x=bss,y=rg125nk$gam,type="l",lwd=2);
points(x=bss,y=rg250nk$gam,type="l",lwd=2);
points(x=bss,y=rg500nk$gam,type="l",lwd=2);
text(x=bss[12],y=rg000nk$gam[12]+0.002,labels="r=0",srt=-0,cex=1.25);
text(x=bss[10],y=rg125nk$gam[10]+0.002,labels="r=1/8",srt=-30,cex=1.25);
text(x=bss[7],y=rg250nk$gam[7]+0.003,labels="r=1/4",srt=-60,cex=1.25);
text(x=bss[4]+0.05,y=rg500nk$gam[4]+0.003,labels="r=1/2",srt=-75,cex=1.25);
#text(x=4.95,y=0.215,labels="B",cex=2.5);


# Finds the beta at which a particular r value intersects r=0 for fitness
findbeta <- function(high.guess,rval,m=FALSE,mmin=1,c=1){
    low.guess <- 0;
    outbr <- findm(low.guess=0,high.guess=10,rval=0,mmin=mmin,Beta=0,c=c);
    outgm <- OffATr(r=0,m=outbr,mmin=mmin,Beta=0,c=c) / outbr;
    if(m == FALSE){ # If we're not constraining m
        check  <- 1;
        bguess <- 0.5 * high.guess;
        i      <- 0;
        l      <- low.guess
        u      <- high.guess;
        while(abs(check) > 0.000001 & i < 1000000){
            mst   <- findm(low.guess=0,high.guess=10,rval=rval,mmin=mmin,Beta=bguess,c=c);
            gam   <- OffATr(r=rval,m=mst,mmin=mmin,Beta=bguess,c=c) / mst;
            check <- gam - outgm;
            if(check < 0){
                u      <- bguess;
                bguess <- 0.5*(l+bguess); 
            }else{
                l      <- bguess;
                bguess <- 0.5*(u+bguess);
            }
            i <- i+1;
        }
    }else{ #If we are forcing m to a particular value
        check  <- 1;
        bguess <- 0.5 * high.guess;
        i      <- 0;
        l      <- low.guess
        u      <- high.guess;
        while(abs(check) > 0.000001 & i < 1000000){
            gam   <- OffATr(r=rval,m=m,mmin=mmin,Beta=bguess,c=c) / m;
            check <- gam - outgm;
            if(check < 0){
                u      <- bguess;
                bguess <- 0.5*(l+bguess); 
            }else{
                l      <- bguess;
                bguess <- 0.5*(u+bguess);
            }
            i <- i+1;
        }
    }   
    return(bguess);
}
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    fm  <- function(Beta, m, r=rval){ 
        OffATr(r=rval, m=m, mmin=mmin, Beta=Beta, c=c);
    }
    lg  <- fm(Beta=low.guess, r=rval, m=m1);
    hg  <- fm(Beta=high.guess,r=rval, m=m1);
    if(lg > 0){
        u <- low.guess;
        l <- high.guess;
    }else{
        u <- high.guess;
        l <- low.guess;
    }
    if((fm(l,m1) > 0 & fm(u,m1) > 0) | (fm(l,m1) < 0 & fm(u,m1) < 0)){
        return("Value of m is outside the range");
    }else{
        outbr  <- OffATr(r=0, m=m2, mmin=mmin, Beta=0, c=c) / m2;
        check  <- 1;
        bguess <- 0.5 * (l+u);
        i      <- 0;
        while(abs(check) > 0.000001 & i < 1000000){
            check <- (fm(r=rval, Beta=bguess, m=m1) / m1) - outbr;
            if(check > 0){
                u      <- bguess;
                bguess <- 0.5*(l+bguess); 
            }else{
                l      <- bguess;
                bguess <- 0.5*(u+bguess);
            }
            i <- i+1;
        }
        return(bguess);
    }
} # Running the below returns the estimate






OffATr <- function(r, m, mmin=1, Beta=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - mmin - Beta*r)) ));
}






yacas("Simplify(
      0.5*(1+r)*(1-Exp(-c*(m-z-b*r))) - (r/2)*(1-Exp(-c*(m-i))))")



findtest <- function(low.guess,high.guess,rval,mmin=1,Beta=1,c=1){
    # First need the optimal m for outbreeding below
    #m0  <- findm(low.guess=0,high.guess=10,rval=0,mmin=1,Beta=1,c=1);
    fm  <- function(m,r=rval){ 
        OffATrd(r=rval, mmin=mmin, Beta=Beta, m=m, c=c)*(0-m) + 
            Offtest(r=rval, mmin=mmin, Beta=Beta, m=m, c=c);
    }
    lg  <- fm(m=low.guess, r=rval);
    hg  <- fm(m=high.guess,r=rval);
    if(lg > 0){
        u <- low.guess;
        l <- high.guess;
    }else{
        u <- high.guess;
        l <- low.guess;
    }
    if((fm(l) > 0 & fm(u) > 0) | (fm(l) < 0 & fm(u) < 0)){
        return("Value of m is outside the range");
    }else{
        check  <- 1;
        mguess <- 0.5 * (l+u);
        i      <- 0;
        while(abs(check) > 0.000001 & i < 1000000){
            check <- fm(r=rval, m=mguess);
            if(check > 0){
                u      <- mguess;
                mguess <- 0.5*(l+mguess); 
            }else{
                l      <- mguess;
                mguess <- 0.5*(u+mguess);
            }
            i <- i+1;
        }
        return(mguess);
    }
} # Running the below returns the


function(r, m, mmin=1, Beta=1, c=1){
    return(  0.5 * (1 + r) * (exp(-c * (m - mmin - Beta * r)) * c)  );
    # Also can simplify: return(  (c/2)*(1+r)*exp(-c*(m-mmin-Beta*r))  );
}




ftest  <- expression(0.5*(1+r)*(1-exp(-c*(m-mmin-Beta*r))) - (r/2)*(1-exp(-c*(z-mmin))),'m');
# We can use the above to differentiate fm wrt `m`
ftestd <- D(ftest,'m'); # can print fmd to show solution


Offtest <- function(r, m, mmin=1, Beta=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - mmin - Beta*r)) ) - (r/2)*(1-exp(-c*(2.146194-mmin))));
}



#################################################################
#################################################################
#################################################################
#################################################################

# New equations needed to present the results below
fpair <- expression(0.5*(1+r)*(1-exp(-c*(m-mmin-Beta*r))) - 
                        (r/2)*(1-exp(-c*(z-mmin))),'m');
fpaird <- D(fpair,'m'); # can print fmd to show solution
Offpair <- function(r, m, mmin=1, Beta=1, c=1, m0=r00m){
    direct   <- (1/2)*(1+r) * (1 - exp(-c*(m - mmin - Beta*r)) );
    indirect <- (r/2)*(1-exp(-c*(r00m-mmin)));
    return( direct - indirect);
}
findmpair <- function(low.guess,high.guess,rval,mmin=1,Beta=1,c=1,m0=r00m){
    # Note that derivative of zeta has not changed, only the original f
    fm  <- function(m,r=rval){ 
        OffATrd(r=rval, mmin=mmin, Beta=Beta, m=m, c=c)*(0-m) + 
            Offpair(r=rval, mmin=mmin, Beta=Beta, m=m, c=c, m0=m0);
    }
    lg  <- fm(m=low.guess, r=rval);
    hg  <- fm(m=high.guess,r=rval);
    if(lg > 0){
        u <- low.guess;
        l <- high.guess;
    }else{
        u <- high.guess;
        l <- low.guess;
    }
    if((fm(l) > 0 & fm(u) > 0) | (fm(l) < 0 & fm(u) < 0)){
        return("Value of m is outside the range");
    }else{
        check  <- 1;
        mguess <- 0.5 * (l+u);
        i      <- 0;
        while(abs(check) > 0.000001 & i < 1000000){
            check <- fm(r=rval, m=mguess);
            if(check > 0){
                u      <- mguess;
                mguess <- 0.5*(l+mguess); 
            }else{
                l      <- mguess;
                mguess <- 0.5*(u+mguess);
            }
            i <- i+1;
        }
        return(mguess);
    }
} # Running the below returns the
r05mpB0 <- findmpair(low.guess=0,high.gues=10,rval=0.5,Beta=0);
r05gpB0 <- Offpair(r=0.5, m=r05mpB0, mmin=1, Beta=0, c=1, m0=r00m) / r05mpB0;
r05mpB1 <- findmpair(low.guess=0,high.gues=10,rval=0.5,Beta=1);
r05gpB1 <- Offpair(r=0.5, m=r05mpB0, mmin=1, Beta=1, c=1, m0=r00m) / r05mpB1;



#---------------- Some general patterns now plotted
bss <- seq(from=0.5,to=5,by=0.25);
gamzp <- function(rvl,betas,mmin=1,c=1,m0=r00m){
    mvl <- rep(0,length(betas));
    gam <- rep(0,length(betas));
    for(i in 1:length(bss)){
        mvl[i] <- findmpair(low.guess=0,high.guess=10,rval=rvl,
                            mmin=1,Beta=betas[i],c=1,m0=r00m);
        gam[i] <- Offpair(r=rvl,m=mvl[i],mmin=1,Beta=betas[i],c=1,m0=m0) / mvl[i];
    }
    return(list(mvl=mvl,gam=gam));
}
rg000p <- gamzp(rvl=0.000,betas=bss);
rg125p <- gamzp(rvl=0.125,betas=bss);
rg250p <- gamzp(rvl=0.250,betas=bss);
rg500p <- gamzp(rvl=0.500,betas=bss);
par(mfrow=c(2,1),mar=c(0.25,5,1,1),lwd=2);
layout(matrix(data=c(1,2), nrow=2, ncol=1, byrow = TRUE),
       widths=c(1,1), heights=c(1,1))
#----------------------------------------------------------------
par(mar=c(1,5,2,1),lwd=2);
plot(x=bss,y=rg000p$mvl,type="l",lwd=2,ylim=c(2,6),pch=1.5,xaxt="n",
     xlab="",
     ylab=expression(paste("Optimum parental investment (",italic(m),"*)")),
     cex.lab=1.5,cex.axis=1.5);
points(x=bss,y=rg125p$mvl,type="l",lwd=2);
points(x=bss,y=rg250p$mvl,type="l",lwd=2);
points(x=bss,y=rg500p$mvl,type="l",lwd=2);
points(x=bss,y=rg125$mvl,type="l",lwd=2,lty="dotted",col="grey70");
points(x=bss,y=rg250$mvl,type="l",lwd=2,lty="dotted",col="grey70");
points(x=bss,y=rg500$mvl,type="l",lwd=2,lty="dotted",col="grey70");
text(x=bss[17],y=rg000p$mvl[17]+0.15,labels="r=0",srt=-0,cex=1.25);
text(x=bss[17],y=rg125p$mvl[17]+0.15,labels="r=1/8",srt=6,cex=1.25);
text(x=bss[17],y=rg250p$mvl[17]+0.15,labels="r=1/4",srt=12,cex=1.25);
text(x=bss[17],y=rg500p$mvl[17]+0.15,labels="r=1/2",srt=18,cex=1.25);
text(x=bss[18],y=rg125$mvl[18]-0.15,labels="r=1/8",srt=6,cex=0.8,col="grey70");
text(x=bss[18],y=rg250$mvl[18]-0.15,labels="r=1/4",srt=12,cex=0.8,col="grey70");
text(x=bss[18],y=rg500$mvl[18]-0.15,labels="r=1/2",srt=18,cex=0.8,col="grey70");
text(x=4.95,y=5.9,labels="A",cex=2.5);
#----------------------------------------------------------------
par(mar=c(5,5,0.25,1),lwd=2);
plot(x=bss,y=rg000p$gam,type="l",lwd=2,ylim=c(0.08,0.22),pch=1.5,yaxt="n",
     xlab=expression(paste("Inbreeding depression (",beta,")")),
     ylab=expression(paste("Rate of fitness increase (",gamma,"*)")),
     cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.12,0.16,0.20),cex.axis=1.5);
points(x=bss,y=rg125p$gam,type="l",lwd=2);
points(x=bss,y=rg250p$gam,type="l",lwd=2);
points(x=bss,y=rg500p$gam,type="l",lwd=2);
points(x=bss,y=rg125$gam,type="l",lwd=2,lty="dotted",col="grey70");
points(x=bss,y=rg250$gam,type="l",lwd=2,lty="dotted",col="grey70");
points(x=bss,y=rg500$gam,type="l",lwd=2,lty="dotted",col="grey70");
text(x=bss[17],y=rg000p$gam[17]+0.005,labels="r=0",srt=-0,cex=1.25);
text(x=bss[17],y=rg125p$gam[17]+0.005,labels="r=1/8",srt=-4,cex=1.25);
text(x=bss[17],y=rg250p$gam[17]+0.005,labels="r=1/4",srt=-6,cex=1.25);
text(x=bss[17],y=rg500p$gam[17]+0.005,labels="r=1/2",srt=-8,cex=1.25);
text(x=bss[2],y=rg125$gam[2]+0.004,labels="r=1/8",srt=-7,cex=0.8,col="grey70");
text(x=bss[2],y=rg250$gam[2]+0.005,labels="r=1/4",srt=-12,cex=0.8,col="grey70");
text(x=bss[2],y=rg500$gam[2]+0.005,labels="r=1/2",srt=-23,cex=0.8,col="grey70");
text(x=4.95,y=0.215,labels="B",cex=2.5);





fm  <- expression(0.5*(1+r) * (1 - exp(-c*(m-mmin-Beta*r))) * (1-r*z),'m');
# We can use the above to differentiate fm wrt `m`
fmd <- D(fm,'m'); # can print fmd to show solution


yacas("D(r)(-Ln(-(Exp(-c*(m-mmin-b*r))*r*m*c+Exp(-c*(m-mmin-b*r))*r+
      Exp(-c*(m-mmin-b*r))*m*c+Exp(-c*(m-mmin-b*r))-(r+1))/(r+1-(Exp(-c*(m-mmin-b*r))+
      Exp(-c*(m-mmin-b*r))*m*c+Exp(-c*(m-mmin-b*r))*r+
      Exp(-c*(m-mmin-b*r))*r*m*c)))/(r*b))")




yacas("Solve(0 == 0.5 * (1 + r) * (Exp(-c * (m - mmin - Beta * r)) * c) * (1 - 
    z)*(0-m) + 0.5*(1+r) * (1 - Exp(-c*(m-mmin-Beta*r))) * (1-z) ,m)")








yacas("Simplify(-(0.5*(r+1)*Exp(-c*(m-mmin-Beta*r))*c*m-0.5*(r+1)*(1-Exp(-c*(m-mmin-Beta*r))))/
      (0.5*(r+1)*(1-Exp(-c*(m-mmin-Beta*r)))-0.5*(r+1)*Exp(-c*(m-mmin-Beta*r))*c*m) )")








yacas("D(m)(-(0.5*(r+1)*Exp(-c*(m-mmin-Beta*r))*c*m-0.5*(r+1)*
      (1-Exp(-c*(m-mmin-Beta*r))))/(0.5*(r+1)*(1-Exp(-c*(m-mmin-Beta*r)))-
      0.5*(r+1)*Exp(-c*(m-mmin-Beta*r))*c*m))")


yacas("Simplify(-(0.5*r*Exp(-c*(m-mmin-Beta*r))*c*m+0.5*r*Exp(-c*(m-mmin-Beta*r))-0.5*r+0.5*Exp(-c*(m-mmin-Beta*r))*c*m+0.5*Exp(-c*(m-mmin-Beta*r))-0.5)/((-0.5)*r*Exp(-c*(m-mmin-Beta*r))*c*m+(-0.5)*r*Exp(-c*(m-mmin-Beta*r))+0.5*r+(-0.5)*Exp(-c*(m-mmin-Beta*r))*c*m-0.5*Exp(-c*(m-mmin-Beta*r))+0.5),z)")


yacas("Simplify(-(0.5*(r+1)*Exp(-c*(m-mmin-Beta*r))*c*m-0.5*(r+1)*(1-Exp(-c*(m-mmin-Beta*r))))/(0.5*(r+1)*(1-Exp(-c*(m-mmin-Beta*r)))-0.5*(r+1)*Exp(-c*(m-mmin-Beta*r))*c*m))")


Offtest <- function(r, m, mmin=1, Beta=1, c=1, z=0){
    return( 0.5*(1+r) * (1-exp(-c*(m-mmin-Beta*r) - z*Beta*r)) );
}



y1 <- Offtest(m=PI,r=0.5,z=1, Beta=1);
plot(x=PI,y=y1,type="l",lwd=2,ylim=c(0,1));





Offtest <- function(r, m, mmin=1, Beta=1, c=1, z=0){
    return( 0.5*(1+r) * (1 - exp(-c*(m-mmin-Beta*r))) * (1-r*z) );
}

Offtestd <- function(r, m, mmin=1, Beta=1, c=1, z=0){
    return( 0.5 * (1 + r) * (exp(-c * (m - mmin - Beta * r)) * c) * (1 -  r * z) );
}


y1 <- Offtest(m=PI,r=0.5,z=0, Beta=1);
plot(x=PI,y=y1,type="l",lwd=2,ylim=c(0,1));


y1 <- Offtest(m=PI,r=0.5,z=0.1, Beta=1);
points(x=PI,y=y1,type="l",lwd=2,ylim=c(0,1),col="red");




findmDelta <- function(low.guess,high.guess,rval,mmin=1,Beta=1,c=1,zed){
    fm  <- function(m,r=rval, z=zed){ 
        Offtestd(r=rval, mmin=mmin, Beta=Beta, m=m, c=c, z=z)*(0-m) + 
            Offtest(r=rval, mmin=mmin, Beta=Beta, m=m, c=c, z=z);
    }
    lg  <- fm(m=low.guess, r=rval, z=zed);
    hg  <- fm(m=high.guess,r=rval, z=zed);
    if(lg > 0){
        u <- low.guess;
        l <- high.guess;
    }else{
        u <- high.guess;
        l <- low.guess;
    }
    if((fm(l) > 0 & fm(u) > 0) | (fm(l) < 0 & fm(u) < 0)){
        return("Value of m is outside the range");
    }else{
        check  <- 1;
        mguess <- 0.5 * (l+u);
        i      <- 0;
        while(abs(check) > 0.000001 & i < 1000000){
            check <- fm(r=rval, m=mguess);
            if(check > 0){
                u      <- mguess;
                mguess <- 0.5*(l+mguess); 
            }else{
                l      <- mguess;
                mguess <- 0.5*(u+mguess);
            }
            i <- i+1;
        }
        return(mguess);
    }
} # Running the below returns the estimate

Beta <- 1;
c    <- 1;
mmin <- 1;
rva  <- 0.5;
zed  <- 0;
y1 <- Offtest(m=PI,r=0.5,z=zed, Beta=1);
plot(x=PI,y=y1,type="l",lwd=2,ylim=c(0,1));
zm <- findm(low.guess=0,high.guess=10,rval=0.5,zed=zed);
gz <- Offtest(r=rva, mmin=mmin, Beta=Beta, m=zm, c=c, z=zed) / zm;
abline(a=0,b=gz,col="black");

zed <- 0.25;
y1 <- Offtest(m=PI,r=0.5,z=zed, Beta=1);
points(x=PI,y=y1,type="l",lwd=2,ylim=c(0,1),col="red");
zm <- findm(low.guess=0,high.guess=10,rval=0.5,zed=zed);
gz <- Offtest(r=rva, mmin=mmin, Beta=Beta, m=zm, c=c, z=zed) / zm;
abline(a=0,b=gz,col="red");

zed <- 0.5;
y1 <- Offtest(m=PI,r=0.5,z=zed, Beta=1);
points(x=PI,y=y1,type="l",lwd=2,ylim=c(0,1),col="blue");
zm <- findm(low.guess=0,high.guess=10,rval=0.5,zed=zed);
gz <- Offtest(r=rva, mmin=mmin, Beta=Beta, m=zm, c=c, z=zed) / zm;
abline(a=0,b=gz,col="blue");

abline(v=zm,lty="dotted")


fm  <- expression(0.5*(1+r) * (1 - exp(-c*(m-mmin-Beta*r))) * (1-r*Delta),'m');
# We can use the above to differentiate fm wrt `m`
fmd <- D(fm,'m'); # can print fmd to show solution
OffDelta <- function(r, m, mmin=1, Beta=1, c=1, Delta){
    return( 0.5*(1+r) * (1 - exp(-c*(m-mmin-Beta*r))) * (1-r*Delta) );
}
OffDeltad <- function(r, m, mmin=1, Beta=1, c=1, Delta){
    return( 0.5 * (1 + r) * (exp(-c * (m - mmin - Beta * r)) * c)*(1-r*Delta) );
}
findmDelta <- function(low.guess,high.guess,rval,mmin=1,Beta=1,c=1,Delta){
    fm  <- function(m,r=rval, DD=Delta){ 
        OffDeltad(r=rval, mmin=mmin, Beta=Beta, m=m, c=c, Delta=Delta)*(0-m) + 
            OffDelta(r=rval, mmin=mmin, Beta=Beta, m=m, c=c, Delta=Delta);
    }
    lg  <- fm(m=low.guess, r=rval, DD=Delta);
    hg  <- fm(m=high.guess,r=rval, DD=Delta);
    if(lg > 0){
        u <- low.guess;
        l <- high.guess;
    }else{
        u <- high.guess;
        l <- low.guess;
    }
    if((fm(l) > 0 & fm(u) > 0) | (fm(l) < 0 & fm(u) < 0)){
        return("Value of m is outside the range");
    }else{
        check  <- 1;
        mguess <- 0.5 * (l+u);
        i      <- 0;
        while(abs(check) > 0.000001 & i < 1000000){
            check <- fm(r=rval, m=mguess);
            if(check > 0){
                u      <- mguess;
                mguess <- 0.5*(l+mguess); 
            }else{
                l      <- mguess;
                mguess <- 0.5*(u+mguess);
            }
            i <- i+1;
        }
        return(mguess);
    }
} # Running the below returns the estimate
Beta <- 1;
c    <- 1;
mmin <- 1;
rva  <- 0.5;
Delt <- 0; # First show no Delta
Del1 <- OffDelta(m=PI,r=0.5,Delta=Delt, Beta=1);
plot(x=PI,y=Del1,type="l",lwd=2,ylim=c(0,1));
d1m <- findmDelta(low.guess=0,high.guess=10,rval=0.5,Delta=Delt);
d1g <- OffDelta(r=rva, mmin=mmin, Beta=Beta, m=d1m, c=c, Delta=Delt) / d1m;
abline(a=0,b=d1g,col="black");
Delt <- 0.5; # Now show significant delta
Del2 <- OffDelta(m=PI,r=0.5,Delta=Delt, Beta=1);
points(x=PI,y=Del2,type="l",lwd=2,ylim=c(0,1),lty="dashed");
d2m <- findmDelta(low.guess=0,high.guess=10,rval=0.5,Delta=Delt);
d2g <- OffDelta(r=rva, mmin=mmin, Beta=Beta, m=d2m, c=c, Delta=Delt) / d2m;
abline(a=0,b=d2g,lty="dashed");










# Below is a function of the general equation for plotting
OffATr <- function(r, m, mmin=1, Beta=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - mmin - Beta*r)) ));
}
PI  <- seq(from=0,to=4,by=0.01);
All <- OffATr(r=0, m=PI, mmin=1, Beta=0, c=1);
par(mar=c(5,5,2,4));
plot(PI,All,type="l",lwd=4,ylim=c(0,1),xlim=c(0,4),
     xlab=expression(paste("Parental investment (",italic(m),")")),
     ylab=expression(paste("Offspring fitness")),
     cex.lab=1.25,cex.axis=1.5);
par(new=TRUE);
plot(x=PI,y=10/PI,ylim=c(0,10),lwd=0,xaxt="n",yaxt="n",
     xlab="",ylab="",lty="dashed",type="l",col="white");
axis(4, cex.axis=0, col.axis="black");
abline(h=0,lty="dotted",lwd=0.8);


# Below is a function of the general equation for plotting
OffATr <- function(r, m, mmin=1, Beta=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - mmin - Beta*r)) ));
}
PI  <- seq(from=0,to=4,by=0.01);
All <- OffATr(r=0, m=PI, mmin=1, Beta=0, c=1);
par(mar=c(5,5,2,4));
plot(PI,All,type="l",lwd=4,ylim=c(0,1),xlim=c(0,4),
     xlab=expression(paste("Parental investment (",italic(m),")")),
     ylab=expression(paste("Offspring fitness")),
     cex.lab=1.25,cex.axis=1.5);
par(new=TRUE);
plot(x=PI[PI>=1],y=10/PI[PI>=1],ylim=c(0,10),lwd=2,xaxt="n",yaxt="n",xlim=c(0,4),
     xlab="",ylab="",lty="dashed",type="l",col="black");
axis(4, cex.axis=1.5, col.axis="black", at=c(2,4,6,8,10));
mtext(text=expression(paste("Offspring produced (",italic(n),")")),
      side=4,line=3, cex=1.5, col="black");
text(x=3.5,y=9.5,labels="M=10",cex=3);
abline(h=0,lty="dotted",lwd=0.8);


