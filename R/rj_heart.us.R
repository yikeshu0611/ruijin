#' clear heart ultra sound data
#'
#' @param dir dir
#'
#' @return dataframe
#' @export
#'
rj_heart.us <- function(dir) {
    if (missing(dir)) dir = getwd()
    filenames = list.files(path = dir)
    DF = NULL
    for (j in 1:length(filenames)) {
        if (j == 1)
            df = NULL
        cat(length(filenames), ':', j, '\n')
        file = filenames[j]
        dd1 = suppressWarnings(readLines(paste0(dir, '/', file)))
        dd2 = paste0(dd1, collapse = "")
        dd3 = stringi::stri_trans_nfkd(dd2)
        dd4 = do::Replace(dd3, pattern = ' {1,}: ')
        ######1
        if (grepl(tmcn::toUTF8('\u4E3B\u52A8\u8109\u6839\u90E8\u5185\u5F84'),
                  dd4)) {
            index1 = do::Replace0(dd4, c(
                tmcn::toUTF8(
                    '.*\u4E3B\u52A8\u8109\u6839\u90E8\u5185\u5F84 {0,}'
                ),
                ' .*'
            ))
        } else{
            index1 = NA
        }
        ######2
        if (grepl(tmcn::toUTF8('\u5DE6\u623F\u5185\u5F84'), dd4)) {
            index2 = do::Replace0(dd4, c(
                tmcn::toUTF8('.*\u5DE6\u623F\u5185\u5F84 {0,}'),
                ' .*'
            ))
        } else{
            index2 = NA
        }
        #####3
        if (grepl(tmcn::toUTF8('\u5DE6\u5BA4\u8212\u5F20\u672B\u671F\u5185\u5F84'),
                  dd4)) {
            index3 = do::Replace0(dd4, c(
                tmcn::toUTF8(
                    '.*\u5DE6\u5BA4\u8212\u5F20\u672B\u671F\u5185\u5F84 {0,}'
                ),
                ' .*'
            ))
        } else{
            index3 = NA
        }

        #####4
        if (grepl(tmcn::toUTF8('\u5DE6\u5BA4\u6536\u7F29\u672B\u671F\u5185\u5F84'),
                  dd4)) {
            index4 = do::Replace0(dd4, c(
                tmcn::toUTF8(
                    '.*\u5DE6\u5BA4\u6536\u7F29\u672B\u671F\u5185\u5F84 {0,}'
                ),
                ' .*'
            ))
        } else{
            index4 = NA
        }
        #####5
        if (grepl(tmcn::toUTF8('\u5BA4\u95F4\u9694\u539A\u5EA6'),
                  dd4)) {
            index5 = do::Replace0(dd4, c(
                tmcn::toUTF8('.*\u5BA4\u95F4\u9694\u539A\u5EA6 {0,}'),
                ' .*'
            ))
        } else{
            index5 = NA
        }

        #####6
        if (grepl(tmcn::toUTF8('\u5DE6\u5BA4\u540E\u58C1\u539A\u5EA6'),
                  dd4)) {
            index6 = do::Replace0(dd4, c(
                tmcn::toUTF8('.*\u5DE6\u5BA4\u540E\u58C1\u539A\u5EA6 {0,}'),
                ' .*'
            ))
        } else{
            index6 = NA
        }
        index6
        #####7
        if (grepl(tmcn::toUTF8('\u5DE6\u5BA4\u5C04\u8840\u5206\u6570\\(%\\)'),
                  dd4)) {
            index7 = do::Replace0(dd4, c(
                tmcn::toUTF8(
                    '.*\u5DE6\u5BA4\u5C04\u8840\u5206\u6570\\(%\\) {0,}'
                ),
                ' .*'
            ))
        } else{
            index7 = NA
        }
        index7
        #####8
        if (grepl(tmcn::toUTF8('\u80BA\u52A8\u8109\u6536\u7F29\u538B'),
                  dd4)) {
            index8 = do::Replace0(dd4, c(
                tmcn::toUTF8(
                    '.*\u4F30\u6D4B\u80BA\u52A8\u8109\u6536\u7F29\u538B\u7EA6 {0,}'
                ),
                'mmHg.*'
            ))
        } else{
            index8 = NA
        }
        index8
        #####9
        if (grepl('E=',
                  dd4)) {
            index9 = do::Replace0(dd4, c(tmcn::toUTF8('.*E= {0,}'),
                                         'cm.*'))
        } else{
            index9 = NA
        }
        index9
        #####10
        if (grepl(', A=',
                  dd4)) {
            index10 = do::Replace0(dd4, c(tmcn::toUTF8('.*, A= {0,}'),
                                          'cm.*'))
        } else{
            index10 = NA
        }
        index10
        #####11
        if (grepl(tmcn::toUTF8('\u8BCA\u65AD\u610F\u89C1'),
                  dd4)) {
            index11 = do::Replace0(dd4, c(
                tmcn::toUTF8('.*\u8BCA\u65AD\u610F\u89C1 {0,}'),
                '.*:'
            ))
        } else{
            index11 = NA
        }
        index11
        df.i = data.frame(
            id = do::Replace0(file, '.txt'),
            index1,
            index2,
            index3,
            index4,
            index5,
            index6,
            index7,
            index8,
            index9,
            index10,
            index11
        )
        df = rbind(df, df.i)
    }
    index.names = c(
        tmcn::toUTF8('\u4E3B\u52A8\u8109\u6839\u90E8\u5185\u5F84'),
        tmcn::toUTF8('\u5DE6\u623F\u5185\u5F84'),
        tmcn::toUTF8('\u5DE6\u5BA4\u8212\u5F20\u672B\u671F\u5185\u5F84'),
        tmcn::toUTF8('\u5DE6\u5BA4\u6536\u7F29\u672B\u671F\u5185\u5F84'),
        tmcn::toUTF8('\u5BA4\u95F4\u9694\u539A\u5EA6'),
        tmcn::toUTF8('\u5DE6\u5BA4\u540E\u58C1\u539A\u5EA6'),
        tmcn::toUTF8('\u5DE6\u5BA4\u5C04\u8840\u5206\u6570(%)'),
        tmcn::toUTF8('\u80BA\u52A8\u8109\u6536\u7F29\u538B'),
        tmcn::toUTF8('E\u5CF0'),
        tmcn::toUTF8('A\u5CF0'),
        tmcn::toUTF8('\u8BCA\u65AD\u610F\u89C1')
    )
    colnames(df) = c(tmcn::toUTF8('\u4F4F\u9662\u53F7'),
                     index.names)

    df
}
