#' query lab test data
#'
#' @param data data after rj_labtest.getdata
#' @param query one or more query words
#'
#' @return dataframe
#' @export
#'
rj_labtest.query<-function(data,query){
    data=as.data.frame(data)
    query=stringi::stri_trans_nfkd(query)
    id=data[,tmcn::toUTF8('\u4F4F\u9662\u53F7')]
    id.u=as.character(unique(id))
    for (i in 1:length(id.u)) {
        if (i==1) df=NULL
        data.i=data[id==id.u[i],]
        location=do::`%==%`(query,data.i[,tmcn::toUTF8('\u9879\u76EE')])
        if (any(location=='integer(0)')){
            names.integer0=names(location)[location=='integer(0)']
            message(id.u[i],
                    tmcn::toUTF8(' \u4E2D\u672A\u627E\u5230: '),
                    length(names.integer0),tmcn::toUTF8('\u4E2A\u9879\u76EE'),
                    paste0('\n',
                           paste0(
                               paste0('       ',names.integer0),
                           collapse = '\n')),
                    '\n')
            location=location[location!='integer(0)']
        }
        if (is.atomic(location)) location=list(location)
        location.max=unlist(lapply(location, max))
        df.i=data.i[location.max,]
        df.i2=t(as.character(
            df.i[,tmcn::toUTF8('\u7ED3\u679C')]))
        df.i3=cbind(id.u[i],df.i2)
        rownames(df.i3)=NULL
        colnames(df.i3)=c(tmcn::toUTF8('\u4F4F\u9662\u53F7'),
                          as.character(
                              df.i[,tmcn::toUTF8('\u9879\u76EE')]))
        if (i==1){
            df=df.i3
        }else{
            df=plyr::rbind.fill.matrix(df,df.i3)
        }
    }
    df
    df2=ifelse(is.na(df),"",df)
    df3=data.frame(df2,check.names = FALSE)
    df3
}
