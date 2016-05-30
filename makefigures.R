library("extrafont");                                                                 
library(reshape2);                                                                    
setwd(path.expand("~/Dropbox/DuthieManu/Inbreeding_Care"));         


figscale <- 1.0; # Avoid making huge figures.

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
# -------------------------------------
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
#-----------------------------------------
setEPS():
cairo_ps("mcurves_uni.eps",family="Arial",width=6,height=8);
par(mfrow=c(2,1),mar=c(5,5,1,1),lwd=2);
layout(matrix(data=c(1,2), nrow=2, ncol=1, byrow = TRUE),
       widths=c(1,1), heights=c(1,1));
# --------------------------------------------------------------------
plot(PI,Alleles_IBD_outbr,type="l",lwd=3,ylim=c(0,1),yaxs="i",xaxs="i",
     xlab=expression(paste("Parental investment (",italic(m),")")),
     ylab=expression(paste("IBD alleles in offspring (",zeta[off],")")),
     cex.lab=1.5,cex.axis=1.5);
abline(h=0,lty="dotted",lwd=0.8);
points(PI,Alleles_IBD_inbr,type="l",lwd=3,lty="dashed");
abline(a=0,b=r00g,lty="solid", lwd=1); # Tangent line r = 0
abline(a=0,b=r05g,lty="dashed",lwd=1); # Tangent line r = 0.5
text(x=0.15,y=0.92,labels="A",cex=2.5);
# --------------------------------------------------------------------
plot(x=bss,y=rg000$mvl,type="l",lwd=2,ylim=c(2,6),pch=1.5,
     xlab=expression(paste("Inbreeding depression (",beta,")")),
     ylab=expression(paste("Optimum PI (",italic(m),"*)")),
     cex.lab=1.5,cex.axis=1.5);
points(x=bss,y=rg125$mvl,type="l",lwd=2);
points(x=bss,y=rg250$mvl,type="l",lwd=2);
points(x=bss,y=rg500$mvl,type="l",lwd=2);
text(x=bss[17],y=rg000$mvl[17]+0.17,labels="r=0",srt=-0,cex=1.25);
text(x=bss[17],y=rg125$mvl[17]+0.2,labels="r=1/8",srt=6,cex=1.25);
text(x=bss[17],y=rg250$mvl[17]+0.2,labels="r=1/4",srt=12.5,cex=1.25);
text(x=bss[17],y=rg500$mvl[17]+0.2,labels="r=1/2",srt=18,cex=1.25);
text(x=0.5,y=5.80,labels="B",cex=2.5);
dev.off();

OffAT <- function(r, m, B0=1, B1=1, c=1){
    return( (1/2)*(1+r) * (1 - exp(-c*(m - B0 - B1*r)) ));
}


# Find gamma assuming investing as if outbreeding
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
# Find gamma assuming investing as if full sibling inbreeding
gamzfsib <- function(rvl,betas,mmin=1,c=1){
    mvl <- rep(0,length(betas));
    gam <- rep(0,length(betas));
    for(i in 1:length(bss)){
        mvl[i] <- findm(low.guess=0,high.guess=10,rval=0.5,mmin=1,Beta=betas[i],c=1);
        gam[i] <- OffATr(r=rvl, m=mvl[i], mmin=1, Beta=betas[i], c=1) / mvl[i];
    }
    return(list(mvl=mvl,gam=gam));
}
rg000fs <- gamzfsib(rvl=0.000,betas=bss);
rg125fs <- gamzfsib(rvl=0.125,betas=bss);
rg250fs <- gamzfsib(rvl=0.250,betas=bss);
rg500fs <- gamzfsib(rvl=0.500,betas=bss);
#--- Start building the figure below
setEPS():
cairo_ps("gammas_uni.eps",family="Arial",width=6,height=8);
par(mfrow=c(2,1),mar=c(0.25,1,4,1),oma=c(5,5,1,1),lwd=2);
#----------------------------------------------------------------
#par(mar=c(0.5,1,0.25,1),lwd=2);
plot(x=bss,y=rg000$gam,type="l",lwd=2,ylim=c(0.09,0.22),pch=1.5,yaxt="n",
     ylab="",xaxt="n",cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.10,0.15,0.20),cex.axis=1.5);
points(x=bss,y=rg125$gam,type="l",lwd=2);
points(x=bss,y=rg250$gam,type="l",lwd=2);
points(x=bss,y=rg500$gam,type="l",lwd=2);
text(x=bss[18],y=rg000$gam[18]+0.0038,labels="r=0",srt=-0,cex=0.8);
text(x=bss[18],y=rg125$gam[18]+0.0038,labels="r=1/8",srt=-7,cex=0.8);
text(x=bss[18],y=rg250$gam[18]+0.0038,labels="r=1/4",srt=-9,cex=0.8);
text(x=bss[18],y=rg500$gam[18]+0.0038,labels="r=1/2",srt=-12,cex=0.8);
text(x=4.95,y=0.213,labels="A",cex=2.5);
#----------------------------------------------------------------
par(mar=c(4,1,0.25,1),lwd=2);
plot(x=bss,y=rg000nk$gam,type="l",lwd=2,ylim=c(0.09,0.22),pch=1.5,yaxt="n",
     xlab=expression(paste("Inbreeding depression (",beta,")")),
     cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.10,0.15,0.20),cex.axis=1.5);
points(x=bss,y=rg125nk$gam,type="l",lwd=2);
points(x=bss,y=rg250nk$gam,type="l",lwd=2);
points(x=bss,y=rg500nk$gam,type="l",lwd=2);
text(x=bss[12],y=rg000nk$gam[12]+0.004,labels="r=0",srt=-0,cex=0.8);
text(x=bss[10],y=rg125nk$gam[10]+0.005,labels="r=1/8",srt=-16,cex=0.8);
text(x=bss[7],y=rg250nk$gam[7]+0.005,labels="r=1/4",srt=-40,cex=0.8);
text(x=bss[4]+0.075,y=rg500nk$gam[4]+0.002,labels="r=1/2",srt=-65,cex=0.8);
text(x=4.95,y=0.213,labels="B",cex=2.5);
mtext(text=expression(paste("Rate of fitness increase (",gamma,")")),
      side=2,line=3, cex=1.5, col="black", outer=TRUE);
dev.off();
#----------------------------------------------------------------
# Next, the below generates a function to find where lines intersect
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
r05gpB1 <- Offpair(r=0.5, m=r05mpB1, mmin=1, Beta=1, c=1, m0=r00m) / r05mpB1;





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
#----------------------------------------------------------------
Alleles_IBD_outbr_pair <- Offpair(r=0.0, m=PI, mmin=1, Beta=1, c=1);
Alleles_IBD_inbr_pair  <- Offpair(r=0.5, m=PI, mmin=1, Beta=1, c=1);
#-----------------------------------------
setEPS():
cairo_ps("mcurves_bip.eps",family="Arial",width=6,height=8);
par(mfrow=c(2,1),mar=c(5,5,1,1),lwd=2);
#layout(matrix(data=c(1,2), nrow=2, ncol=1, byrow = TRUE),
#       widths=c(1,1), heights=c(1,1));
# --------------------------------------------------------------------
plot(PI,Alleles_IBD_outbr_pair,type="l",lwd=3,ylim=c(0,1),yaxs="i",xaxs="i",
     xlab=expression(paste("Parental investment (",italic(m),")")),
     ylab=expression(paste("IBD alleles in offspring (",zeta[off],")")),
     cex.lab=1.5,cex.axis=1.5);
abline(h=0,lty="dotted",lwd=0.8);
points(PI,Alleles_IBD_inbr_pair,type="l",lwd=3,lty="dashed");
abline(a=0,b=r00g,lty="solid", lwd=1); # Tangent line r = 0
abline(a=0,b=r05gpB1,lty="dashed",lwd=1); # Tangent line r = 0.5
text(x=0.15,y=0.925,labels="A",cex=2.5);
# --------------------------------------------------------------------
plot(x=bss,y=rg000$mvl,type="l",lwd=2,ylim=c(2,6),pch=1.5,
     xlab=expression(paste("Inbreeding depression (",beta,")")),
     ylab=expression(paste("Optimum PI (",italic(m),"*)")),
     cex.lab=1.5,cex.axis=1.5);
points(x=bss,y=rg125$mvl,type="l",lwd=2);
points(x=bss,y=rg250$mvl,type="l",lwd=2);
points(x=bss,y=rg500$mvl,type="l",lwd=2);
text(x=bss[17],y=rg000$mvl[17]+0.17,labels="r=0",srt=-0,cex=1.15);
text(x=bss[17],y=rg125$mvl[17]+0.17,labels="r=1/8",srt=6,cex=1.15);
text(x=bss[17],y=rg250$mvl[17]+0.17,labels="r=1/4",srt=12.5,cex=1.15);
text(x=bss[17],y=rg500$mvl[17]+0.17,labels="r=1/2",srt=18,cex=1.15);
text(x=0.5,y=5.85,labels="B",cex=2.5);
dev.off();


#--- Start building the figure below
setEPS():
cairo_ps("gammas_bip.eps",family="Arial",width=6,height=8);
par(mfrow=c(2,1),mar=c(0.25,1,4,1),oma=c(5,5,1,1),lwd=2);
#----------------------------------------------------------------
plot(x=bss,y=rg000p$gam,type="l",lwd=2,ylim=c(0.05,0.18),pch=1.5,yaxt="n",
     xlab=expression(paste("Inbreeding depression (",beta,")")),xaxt="n",
     ylab="",cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.05,0.10,0.15,0.20),cex.axis=1.5);
points(x=bss,y=rg125p$gam,type="l",lwd=2);
points(x=bss,y=rg250p$gam,type="l",lwd=2);
points(x=bss,y=rg500p$gam,type="l",lwd=2);
text(x=bss[17],y=rg000p$gam[17]+0.005,labels="r=0",srt=-0,cex=1);
text(x=bss[17],y=rg125p$gam[17]+0.005,labels="r=1/8",srt=-4,cex=1);
text(x=bss[17],y=rg250p$gam[17]+0.005,labels="r=1/4",srt=-6,cex=1);
text(x=bss[17],y=rg500p$gam[17]+0.005,labels="r=1/2",srt=-8,cex=1);
text(x=4.95,y=0.174,labels="A",cex=2.5);
#----------------------------------------------------------------
par(mar=c(4,1,0.25,1),lwd=2);
r000_000pt <- Offpair(r=0.000, m=r00m, mmin=1, Beta=bss, c=1, m0=r00m) / r00m;
r125_000pt <- Offpair(r=0.125, m=r00m, mmin=1, Beta=bss, c=1, m0=r00m) / r00m;
r250_000pt <- Offpair(r=0.250, m=r00m, mmin=1, Beta=bss, c=1, m0=r00m) / r00m;
r500_000pt <- Offpair(r=0.500, m=r00m, mmin=1, Beta=bss, c=1, m0=r00m) / r00m;
plot(x=bss,y=r000_000pt,type="l",lwd=2,ylim=c(0.05,0.18),pch=1.5,yaxt="n",
     xlab=expression(paste("Inbreeding depression (",beta,")")),
     ylab="",cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.05,0.10,0.15,0.20),cex.axis=1.5);
points(x=bss,y=r125_000pt,type="l",lwd=2);
points(x=bss,y=r250_000pt,type="l",lwd=2);
points(x=bss,y=r500_000pt,type="l",lwd=2);
text(x=bss[12],y=r000_000pt[12]+0.005,labels="r=0",srt=-0,cex=1);
text(x=bss[10],y=r125_000pt[10]+0.005,labels="r=1/8",srt=-15,cex=1);
text(x=bss[7],y=r250_000pt[7]+0.005,labels="r=1/4",srt=-34,cex=1);
text(x=bss[4]+0.06,y=r500_000pt[4]+0.004,labels="r=1/2",srt=-65,cex=1);
text(x=4.95,y=0.174,labels="B",cex=2.5);
#----------------------------------------------------------------
mtext(text=expression(paste("Rate of fitness increase (",gamma,")")),
      side=2,line=2, cex=1.5, col="black",outer=TRUE);
dev.off();




OffATf <- function(f, k, m, B0=1, B1=1, c=1){
    return( (1/2)*(1+(2*k/(1+f))) * (1 - exp(-c*(m - B0 - 2*B1*k)) ));
}
findmf <- function(low.guess,high.guess,kval,fval,B1=1,c=1,B0=1){
    OffATf <- function(m, f=fval, k=kval, B0=1, B1=1, c=1){
        return( (1/2)*(1+(2*k/(1+f))) * (1 - exp(-c*(m - B0 - 2*B1*k)) ));
    }
    OffATff <- function(m, k=kval, f=fval, B0=1, B1=1, c=1){
        return( 0.5 * (1 + (2 * k/(f + 1))) * (exp(-c * (m - B0 - 2 * B1 * k)) * c) );
    }    
    fm <- function(m, f=fval, k=kval, B0=1, B1=1, c=1){
        OffATff(m=m, f=fval, k=kval, B0=B0, B1=B1, c=c)*(0-m) + 
            OffATf(m=m, f=fval, k=kval, B0=B0, B1=B1, c=c);
    }
    lg  <- fm(m=low.guess, k=kval, f=fval);
    hg  <- fm(m=high.guess,k=kval, f=fval);
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
            check <- fm(k=kval, f=fval, m=mguess);
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
imopt.f000 <- findmf(low.guess=0,high.guess=4,kval=0.25,fval=0.0, B1=1);
imopt.f025 <- findmf(low.guess=0,high.guess=4,kval=0.25,fval=0.25, B1=1);
itang.f000 <- OffATf(m=imopt.f000, f=0, k=0.25, B0=1, B1=1, c=1) / imopt.f000;
itang.f025 <- OffATf(m=imopt.f025, f=0.25, k=0.25, B0=1, B1=1, c=1) / imopt.f025;
omopt.f000 <- findmf(low.guess=0,high.guess=4,kval=0.0,fval=0.0, B1=1);
omopt.f025 <- findmf(low.guess=0,high.guess=4,kval=0.0,fval=0.25, B1=1);
otang.f000 <- OffATf(m=omopt.f000, f=0, k=0.0, B0=1, B1=1, c=1) / omopt.f000;
otang.f025 <- OffATf(m=omopt.f025, f=0.25, k=0.0, B0=1, B1=1, c=1) / omopt.f025;




Alleles_IBD_f000 <- OffATf(k=0.25, f=0.0, m=PI, B0=1, B1=1, c=1);
setEPS():
cairo_ps("inbred_parent.eps",family="Arial");
par(mar=c(5,5,2,2));
plot(PI,Alleles_IBD_f000,type="l",lwd=1,ylim=c(0,0.95),lty="solid",
     xlab=expression(paste("Parental investment (",italic(m),")")),
     ylab=expression(paste("IBD alleles in offspring (",zeta[off],")")),
     cex.lab=1.5,cex.axis=1.5,yaxs="i",xaxs="i");
abline(h=0,lty="dotted",lwd=0.8);
Alleles_IBD_f025 <- OffATf(k=0.25, f=0.25, m=PI, B0=1, B1=1, c=1);
abline(b=itang.f000,a=0,lty="solid",lwd=0.5,col="grey40");
abline(b=itang.f025,a=0,lty="solid",lwd=0.5,col="grey40");
polygon(x=c(PI,rev(PI)),y=c(Alleles_IBD_f000,rev(Alleles_IBD_f025)),col="grey70",border=NA);
points(PI,Alleles_IBD_f025,type="l",lwd=1,col="black",lty="solid");
points(PI,Alleles_IBD_f000,type="l",lwd=1,col="black",lty="solid");
yof000 <- OffATf(k=0.25, f=0.0,  m=imopt.f000, B0=1, B1=1, c=1);
yof025 <- OffATf(k=0.25, f=0.25, m=imopt.f025, B0=1, B1=1, c=1);
xline  <- seq(from=1,to=imopt.f000,by=0.001);
points(x=xline,y=rep(yof000,length(xline)),type="l",lwd=1);
points(x=xline,y=rep(yof025,length(xline)),type="l",lwd=1);
yline  <- seq(from=0,to=yof025,by=0.001);
points(x=rep(imopt.f025,length(yline)),y=yline,type="l",lwd=1);
text(x=3.5,y=0.255,labels=expression(paste(italic(m^{"*"}))),cex=1.75);
arrows(x0=3.345,y0=0.205,x1=imopt.f025+0.04,y1=0.01,length = 0.15,angle=30,code=2,lwd=2);
text(x=0.75,y=0.8,labels=expression(paste(italic(f==0))),cex=1.25);
arrows(x0=0.8,y0=0.75,x1=1.1,y1=yof000+0.01,length = 0.15,angle=30,code=2,lwd=2);
text(x=0.67,y=0.35,labels=expression(paste(italic(f==frac(1,4)))),cex=1.25);
arrows(x0=0.87,y0=0.36,x1=1.1,y1=yof025-0.01,length = 0.15,angle=30,code=2,lwd=2);
dev.off();


fm <- expression(0.5*(1+r)*(1-exp(-c*(m-B0-B1*r))),'m');
fm1 <- D(fm,'m');

r00m.y <- OffATr(r=0,  m=r00m, mmin=1, Beta=1, c=1);
r05m.y <- OffATr(r=0.5, m=r05m, mmin=1, Beta=1, c=1);

rrmm  <- expression((1/beta)*(mopt-mmin+(1/c)*log(1/(mopt*c+1))),'mopt');
# We can use the above to differentiate fm wrt `m`
drdm <- D(rrmm,'mopt'); # can print fmd to show solution
# Note that drdm simplifies considerably from what R produces




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
setEPS();
cairo_ps("DELTA.eps",family="Arial");
par(mar=c(5,5,1,1));
plot(x=PI,y=Del1,type="l",lwd=2,ylim=c(0,1),
     xlab=expression(paste("Parental investment (",italic(m),")")),
     ylab=expression(paste("IBD alleles in offspring (",zeta[off],")")),
     cex.lab=1.5,cex.axis=1.5,yaxs="i",xaxs="i");
d1m <- findmDelta(low.guess=0,high.guess=10,rval=0.5,Delta=Delt);
d1g <- OffDelta(r=rva, mmin=mmin, Beta=Beta, m=d1m, c=c, Delta=Delt) / d1m;
abline(a=0,b=d1g,col="black");
Delt <- 0.5; # Now show significant delta
Del2 <- OffDelta(m=PI,r=0.5,Delta=Delt, Beta=1);
points(x=PI,y=Del2,type="l",lwd=2,ylim=c(0,1),lty="dashed");
d2m <- findmDelta(low.guess=0,high.guess=10,rval=0.5,Delta=Delt);
d2g <- OffDelta(r=rva, mmin=mmin, Beta=Beta, m=d2m, c=c, Delta=Delt) / d2m;
abline(a=0,b=d2g,lty="dashed");
dev.off();



setEPS();
cairo_ps("optima.eps",family="Arial",width=8,height=5);
par(oma=c(4,1,1,1));
moBB <- 1;
mvals   <- seq(from=1, to=6, by=0.01);
par(mfrow=c(1,2),mar=c(1,5,2,1),lwd=2);
# First assume that Beta = 1.
mvr000  <- OffATr(r=0.000, m=mvals, mmin=1, Beta=moBB, c=1) / mvals;
mvr125  <- OffATr(r=0.125, m=mvals, mmin=1, Beta=moBB, c=1) / mvals;
mvr250  <- OffATr(r=0.250, m=mvals, mmin=1, Beta=moBB, c=1) / mvals;
mvr500  <- OffATr(r=0.500, m=mvals, mmin=1, Beta=moBB, c=1) / mvals;
mstr000 <- findm(low.guess=0,high.guess=6,rval=0.000,Beta=moBB,c=1,mmin=1);
mstr125 <- findm(low.guess=0,high.guess=6,rval=0.125,Beta=moBB,c=1,mmin=1);
mstr250 <- findm(low.guess=0,high.guess=6,rval=0.250,Beta=moBB,c=1,mmin=1);
mstr500 <- findm(low.guess=0,high.guess=6,rval=0.500,Beta=moBB,c=1,mmin=1);
mstrgm0 <- OffATr(r=0.000, m=mstr000, mmin=1, Beta=moBB, c=1) / mstr000;
mstrgm1 <- OffATr(r=0.125, m=mstr125, mmin=1, Beta=moBB, c=1) / mstr125;
mstrgm2 <- OffATr(r=0.250, m=mstr250, mmin=1, Beta=moBB, c=1) / mstr250;
mstrgm5 <- OffATr(r=0.500, m=mstr500, mmin=1, Beta=moBB, c=1) / mstr500;
plot(x=mvals,y=mvr000,type="l",lwd=3,ylim=c(0.0,0.21),pch=1.5,yaxt="n",
     ylab=expression(paste("Rate of fitness increase (",gamma,")")),
     cex.lab=1.5,cex.axis=1.5,xaxs="i",yaxs="i",xaxt="n");
axis(side=2,at=c(0,0.05,0.10,0.15,0.20),labels=TRUE,cex.axis=1.5);
axis(side=1,at=c(1,2,3,4,5,6),cex.axis=1.5);
points(x=mvals,y=mvr125,type="l",lwd=3);
points(x=mvals,y=mvr250,type="l",lwd=3);
points(x=mvals,y=mvr500,type="l",lwd=3);
points(x=mstr000,y=mstrgm0,cex=1.5,pch=21,bg="grey70");
points(x=mstr125,y=mstrgm1,cex=1.5,pch=21,bg="grey70");
points(x=mstr250,y=mstrgm2,cex=1.5,pch=21,bg="grey70");
points(x=mstr500,y=mstrgm5,cex=1.5,pch=21,bg="grey70");
text(x=mvals[450],y=mvr000[450]+0.005,labels="r=0",  srt=-25,cex=0.8);
text(x=mvals[450],y=mvr125[450]+0.005,labels="r=1/8",srt=-25,cex=0.8);
text(x=mvals[450],y=mvr250[450]+0.005,labels="r=1/4",srt=-25,cex=0.8);
text(x=mvals[450],y=mvr500[450]+0.006,labels="r=1/2",srt=-30,cex=0.8);
text(x=5.0,y=0.198,labels=expression(paste("A; ",beta==1)),cex=1.5);
lines(x=c(4.1,4.1,6),y=c(0.21,0.189,0.189),lwd=2);
#--- Now show the place where Beta = 3.
par(mar=c(1,1,2,4),lwd=2);
moBB    <- 3;
mvr000  <- OffATr(r=0.000, m=mvals, mmin=1, Beta=moBB, c=1) / mvals;
mvr125  <- OffATr(r=0.125, m=mvals, mmin=1, Beta=moBB, c=1) / mvals;
mvr250  <- OffATr(r=0.250, m=mvals, mmin=1, Beta=moBB, c=1) / mvals;
mvr500  <- OffATr(r=0.500, m=mvals, mmin=1, Beta=moBB, c=1) / mvals;
mstr000 <- findm(low.guess=0,high.guess=6,rval=0.000,Beta=moBB,c=1,mmin=1);
mstr125 <- findm(low.guess=0,high.guess=6,rval=0.125,Beta=moBB,c=1,mmin=1);
mstr250 <- findm(low.guess=0,high.guess=6,rval=0.250,Beta=moBB,c=1,mmin=1);
mstr500 <- findm(low.guess=0,high.guess=6,rval=0.500,Beta=moBB,c=1,mmin=1);
mstrgm0 <- OffATr(r=0.000, m=mstr000, mmin=1, Beta=moBB, c=1) / mstr000;
mstrgm1 <- OffATr(r=0.125, m=mstr125, mmin=1, Beta=moBB, c=1) / mstr125;
mstrgm2 <- OffATr(r=0.250, m=mstr250, mmin=1, Beta=moBB, c=1) / mstr250;
mstrgm5 <- OffATr(r=0.500, m=mstr500, mmin=1, Beta=moBB, c=1) / mstr500;
plot(x=mvals,y=mvr000,type="l",lwd=3,ylim=c(0.0,0.21),pch=1.5,yaxt="n",
     xlab="",ylab="", cex.lab=1.5,cex.axis=1.5,xaxs="i",yaxs="i",xaxt="n");
axis(side=1,at=c(1,2,3,4,5,6),cex.axis=1.5);
points(x=mvals,y=mvr125,type="l",lwd=3);
points(x=mvals,y=mvr250,type="l",lwd=3);
points(x=mvals,y=mvr500,type="l",lwd=3);
points(x=mstr000,y=mstrgm0,cex=1.5,pch=21,bg="grey70");
points(x=mstr125,y=mstrgm1,cex=1.5,pch=21,bg="grey70");
points(x=mstr250,y=mstrgm2,cex=1.5,pch=21,bg="grey70");
points(x=mstr500,y=mstrgm5,cex=1.5,pch=21,bg="grey70");
text(x=mvals[450],y=mvr000[450]+0.005,labels="r=0",  srt=-22,cex=0.8);
text(x=mvals[450],y=mvr125[450]+0.005,labels="r=1/8",srt=-22,cex=0.8);
text(x=mvals[450],y=mvr250[450]+0.005,labels="r=1/4",srt=-22,cex=0.8);
text(x=mvals[450],y=mvr500[450]+0.005,labels="r=1/2",srt=-22,cex=0.8);
text(x=5.0,y=0.198,labels=expression(paste("B; ",beta==3)),cex=1.5);
lines(x=c(4.1,4.1,6),y=c(0.21,0.189,0.189),lwd=2);
mtext(expression(paste("Parental investment (",italic(m),")")),
      outer=TRUE,side=1,line=2.1,cex=1.5);
dev.off();




setEPS();
cairo_ps("AllMstars.eps",family="Arial",width=7,height=8);
gamzVrkin <- function(rvl,kin,betas,mmin=1,c=1,oc=0){
    mvl <- rep(0,length(betas));
    gam <- rep(0,length(betas));
    for(i in 1:length(bss)){
        mvl[i] <- findm(low.guess=0,high.guess=10,rval=kin,mmin=1,Beta=betas[i],c=1);
        if(oc==0){
            gam[i] <- OffATr(r=rvl, m=mvl[i], mmin=1, Beta=betas[i], c=1) / mvl[i];
        }else{
            gam[i] <- Offpair(r=rvl, m=mvl[i], mmin=1, Beta=betas[i], c=1, m0=r00m) / mvl[i];
        }
    }
    return(list(mvl=mvl,gam=gam));
} 
#-----------------------------------------------------------
r0500m000 <- gamzVrkin(rvl=0.500,kin=0.000,betas=bss,oc=0); 
r0250m000 <- gamzVrkin(rvl=0.250,kin=0.000,betas=bss,oc=0);
r0125m000 <- gamzVrkin(rvl=0.125,kin=0.000,betas=bss,oc=0);
r0000m000 <- gamzVrkin(rvl=0.000,kin=0.000,betas=bss,oc=0);
#-----------------------------------------------------------
r0500m125 <- gamzVrkin(rvl=0.500,kin=0.125,betas=bss,oc=0);
r0250m125 <- gamzVrkin(rvl=0.250,kin=0.125,betas=bss,oc=0);
r0125m125 <- gamzVrkin(rvl=0.125,kin=0.125,betas=bss,oc=0);
r0000m125 <- gamzVrkin(rvl=0.000,kin=0.125,betas=bss,oc=0);
#-----------------------------------------------------------
r0500m250 <- gamzVrkin(rvl=0.500,kin=0.250,betas=bss,oc=0);
r0250m250 <- gamzVrkin(rvl=0.250,kin=0.250,betas=bss,oc=0);
r0125m250 <- gamzVrkin(rvl=0.125,kin=0.250,betas=bss,oc=0);
r0000m250 <- gamzVrkin(rvl=0.000,kin=0.250,betas=bss,oc=0);
#-----------------------------------------------------------
r0500m500 <- gamzVrkin(rvl=0.500,kin=0.500,betas=bss,oc=0);
r0250m500 <- gamzVrkin(rvl=0.250,kin=0.500,betas=bss,oc=0);
r0125m500 <- gamzVrkin(rvl=0.125,kin=0.500,betas=bss,oc=0);
r0000m500 <- gamzVrkin(rvl=0.000,kin=0.500,betas=bss,oc=0);
#-----------------------------------------------------------
#-----------------------------------------------------------
r0500m000c <- gamzVrkin(rvl=0.500,kin=0.000,betas=bss,oc=1); 
r0250m000c <- gamzVrkin(rvl=0.250,kin=0.000,betas=bss,oc=1);
r0125m000c <- gamzVrkin(rvl=0.125,kin=0.000,betas=bss,oc=1);
r0000m000c <- gamzVrkin(rvl=0.000,kin=0.000,betas=bss,oc=1);
#-----------------------------------------------------------
r0500m125c <- gamzVrkin(rvl=0.500,kin=0.125,betas=bss,oc=1);
r0250m125c <- gamzVrkin(rvl=0.250,kin=0.125,betas=bss,oc=1);
r0125m125c <- gamzVrkin(rvl=0.125,kin=0.125,betas=bss,oc=1);
r0000m125c <- gamzVrkin(rvl=0.000,kin=0.125,betas=bss,oc=1);
#-----------------------------------------------------------
r0500m250c <- gamzVrkin(rvl=0.500,kin=0.250,betas=bss,oc=1);
r0250m250c <- gamzVrkin(rvl=0.250,kin=0.250,betas=bss,oc=1);
r0125m250c <- gamzVrkin(rvl=0.125,kin=0.250,betas=bss,oc=1);
r0000m250c <- gamzVrkin(rvl=0.000,kin=0.250,betas=bss,oc=1);
#-----------------------------------------------------------
r0500m500c <- gamzVrkin(rvl=0.500,kin=0.500,betas=bss,oc=1);
r0250m500c <- gamzVrkin(rvl=0.250,kin=0.500,betas=bss,oc=1);
r0125m500c <- gamzVrkin(rvl=0.125,kin=0.500,betas=bss,oc=1);
r0000m500c <- gamzVrkin(rvl=0.000,kin=0.500,betas=bss,oc=1);
#-----------------------------------------------------------
par(mfrow=c(4,2),mar=c(0.5,0.5,0.5,0.5),oma=c(5.5,5.5,4,1));
#-----------------------------------------------------------
plot(x=bss,y=r0500m000$gam,type="n",lwd=2,ylim=c(0.09,0.22),pch=1.5,yaxt="n",
     xlab="",xaxt="n",ylab="",cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.10,0.15,0.20),cex.axis=1.5);
points(x=bss,y=r0500m000$gam,type="l",lwd=4);
points(x=bss,y=r0250m000$gam,type="l",lwd=3);
points(x=bss,y=r0125m000$gam,type="l",lwd=2);
points(x=bss,y=r0000m000$gam,type="l",lwd=1);
text(x=4.95,y=0.212,labels="A",cex=2.5);
text(x=1.20,y=0.212,labels=expression(paste("m*"[r==0])),cex=1.5);
#-----------------------------------------------------------
plot(x=bss,y=r0500m000c$gam,type="n",lwd=2,ylim=c(0.09,0.22),pch=1.5,yaxt="n",
     xlab="",xaxt="n",ylab="",cex.lab=1.5,cex.axis=1.5);
points(x=bss,y=r0500m000c$gam,type="l",lwd=4);
points(x=bss,y=r0250m000c$gam,type="l",lwd=3);
points(x=bss,y=r0125m000c$gam,type="l",lwd=2);
points(x=bss,y=r0000m000c$gam,type="l",lwd=1);
text(x=4.95,y=0.212,labels="B",cex=2.5);
text(x=1.20,y=0.212,labels=expression(paste("m*"[r==0])),cex=1.5);
#-----------------------------------------------------------
plot(x=bss,y=r0500m125$gam,type="n",lwd=2,ylim=c(0.09,0.22),pch=1.5,yaxt="n",
     xlab="",xaxt="n",ylab="",cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.10,0.15,0.20),cex.axis=1.5);
points(x=bss,y=r0500m125$gam,type="l",lwd=4);
points(x=bss,y=r0250m125$gam,type="l",lwd=3);
points(x=bss,y=r0125m125$gam,type="l",lwd=2);
points(x=bss,y=r0000m125$gam,type="l",lwd=1);
text(x=4.95,y=0.212,labels="C",cex=2.5);
text(x=1.20,y=0.212,labels=expression(paste("m*"[r==1/8])),cex=1.5);
#-----------------------------------------------------------
plot(x=bss,y=r0500m125c$gam,type="n",lwd=2,ylim=c(0.09,0.22),pch=1.5,yaxt="n",
     xlab="",xaxt="n",ylab="",cex.lab=1.5,cex.axis=1.5);
points(x=bss,y=r0500m125c$gam,type="l",lwd=4);
points(x=bss,y=r0250m125c$gam,type="l",lwd=3);
points(x=bss,y=r0125m125c$gam,type="l",lwd=2);
points(x=bss,y=r0000m125c$gam,type="l",lwd=1);
text(x=4.95,y=0.212,labels="D",cex=2.5);
text(x=1.20,y=0.212,labels=expression(paste("m*"[r==1/8])),cex=1.5);
#-----------------------------------------------------------
plot(x=bss,y=r0500m250$gam,type="n",lwd=2,ylim=c(0.09,0.22),pch=1.5,yaxt="n",
     xlab="",xaxt="n",ylab="",cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.10,0.15,0.20),cex.axis=1.5);
points(x=bss,y=r0500m250$gam,type="l",lwd=4);
points(x=bss,y=r0250m250$gam,type="l",lwd=3);
points(x=bss,y=r0125m250$gam,type="l",lwd=2);
points(x=bss,y=r0000m250$gam,type="l",lwd=1);
text(x=4.95,y=0.212,labels="E",cex=2.5);
text(x=1.20,y=0.212,labels=expression(paste("m*"[r==1/4])),cex=1.5);
#-----------------------------------------------------------
plot(x=bss,y=r0500m250c$gam,type="n",lwd=2,ylim=c(0.09,0.22),pch=1.5,yaxt="n",
     xlab="",xaxt="n",ylab="",cex.lab=1.5,cex.axis=1.5);
points(x=bss,y=r0500m250c$gam,type="l",lwd=4);
points(x=bss,y=r0250m250c$gam,type="l",lwd=3);
points(x=bss,y=r0125m250c$gam,type="l",lwd=2);
points(x=bss,y=r0000m250c$gam,type="l",lwd=1);
text(x=4.95,y=0.212,labels="F",cex=2.5);
text(x=1.20,y=0.212,labels=expression(paste("m*"[r==1/4])),cex=1.5);
#-----------------------------------------------------------
plot(x=bss,y=r0500m500$gam,type="n",lwd=2,ylim=c(0.09,0.22),pch=1.5,yaxt="n",
     xlab="", ylab="",cex.lab=1.5,cex.axis=1.5);
axis(side=2,at=c(0.10,0.15,0.20),cex.axis=1.5);
points(x=bss,y=r0500m500$gam,type="l",lwd=4);
points(x=bss,y=r0250m500$gam,type="l",lwd=3);
points(x=bss,y=r0125m500$gam,type="l",lwd=2);
points(x=bss,y=r0000m500$gam,type="l",lwd=1);
text(x=4.95,y=0.212,labels="G",cex=2.5);
text(x=1.20,y=0.212,labels=expression(paste("m*"[r==1/2])),cex=1.5);
#-----------------------------------------------------------
plot(x=bss,y=r0500m500$gam,type="n",lwd=2,ylim=c(0.09,0.22),pch=1.5,yaxt="n",
     xlab="",ylab="",cex.lab=1.5,cex.axis=1.5);
points(x=bss,y=r0500m500c$gam,type="l",lwd=4);
points(x=bss,y=r0250m500c$gam,type="l",lwd=3);
points(x=bss,y=r0125m500c$gam,type="l",lwd=2);
points(x=bss,y=r0000m500c$gam,type="l",lwd=1);
text(x=4.95,y=0.212,labels="H",cex=2.5);
text(x=1.20,y=0.212,labels=expression(paste("m*"[r==1/2])),cex=1.5);
#-----------------------------------------------------------
mtext(text=expression(paste("Inbreeding depression (",beta,")")),
      side=1,line=3, cex=1.5, col="black" , outer=TRUE);
mtext(text=expression(paste("Rate of fitness increase (",gamma,")")),
      side=2,line=2, cex=1.5, col="black", outer=TRUE);
mtext(text="Single parent", adj=0,
      side=3,line=1, cex=1.5, col="black", outer=TRUE);
mtext(text="Strict monogamy", adj=1,
      side=3,line=1, cex=1.5, col="black", outer=TRUE);

dev.off();











