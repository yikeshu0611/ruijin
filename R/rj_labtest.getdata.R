#' get data for lab test
#'
#' @param dir directory of labe test text
#'
#' @return df
#' @export
#'
rj_labtest.getdata <- function(dir){
    if (missing(dir)) dir=getwd()
    filenames=list.files(path = dir)
    DF=NULL
    for (j in 1:length(filenames)) {
        cat(length(filenames),':',j,'\n')
        file=filenames[j]
        dd1=suppressWarnings(readLines(paste0(dir,'/',file)))
        dd2=stringi::stri_trans_nfkd(dd1)
        date=stringr::str_extract_all(
            pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}",
            string = dd2)[[1]]
        dd3=do::Replace(data = dd2,
                        from = "[0-9]{4}-[0-9]{2}-[0-9]{2}",
                        to='thisisforsplit',
                        pattern = c(' {1,}: ',
                                    ' {1,}pg/ml:pg/ml'))
        dd4=strsplit(x = dd3,split = 'thisisforsplit')[[1]]
        dd5=dd4[nchar(dd4) != 0]
        for (i in 1:length(date)) {
            if (i==1) df.3col=NULL
            #skip in some condition
            if (length(do::`%s=%`('\u5C3F\u6C89\u6E23\u5B9A\u91CF\u68C0\u6D4B',dd5[i])[[1]]) !=0) next(i)
            dd.i=do::Replace0(data = dd5[i],
                              from = tmcn::toUTF8('\u4E2A\u4F53\u5316\u7528\u836F\u5EFA\u8BAE.*'))
            dd.i=strsplit(x = dd.i,split = ',')[[1]]
            line1df=to.df_1line(x = dd.i,split = ' ',
                                paste0(file,': ',i))
            line1df
            df.3col.i=cbind(id=do::Replace0(file,'.txt'),
                            date=date[i],line1df)
            df.3col=rbind(df.3col,df.3col.i)
        }
        DF=rbind(DF,df.3col)
    }
    colnames(DF)=c(tmcn::toUTF8('\u4F4F\u9662\u53F7'),
                   tmcn::toUTF8('\u68C0\u6D4B\u65E5\u671F'),
                   tmcn::toUTF8('\u9879\u76EE'),
                   tmcn::toUTF8('\u7ED3\u679C'))
    DF
}
