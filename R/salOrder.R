#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' salOrderUploadServer()
salOrderUploadServer <- function(input,output,session,dms_token) {

  options(shiny.maxRequestSize = 30 * 1024^2)
  #获取参数
  text_salOrder_upload = tsui::var_file('text_salOrder_upload')

  shiny::observeEvent(input$btn_salOrder_upload,{

    filename=text_salOrder_upload()

    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{

      # 清空临时表

      mdlDFSalOrderUploadPkg::salOrder_delete(dms_token = dms_token)


      data <- readxl::read_excel(filename,col_types =  c("text", "text", "text",
                                                         "text", "text", "text", "text", "text",
                                                         "text", "text", "text", "text", "text",
                                                         "text", "text", "text", "text", "text",
                                                         "text", "text", "text", "text", "text",
                                                         "numeric", "text", "text", "text",
                                                         "text", "text", "text", "text", "text",
                                                         "text", "text", "text", "text", "text",
                                                         "text"))



      data = as.data.frame(data)
      data = tsdo::na_standard(data)





      tsda::mysql_writeTable2(token = dms_token,table_name = 'rds_erp_byd_src_t_sal_saleorder_list_input',r_object = data,append = TRUE)

      # 插入list表和表头表体

      mdlDFSalOrderUploadPkg::salOrder_insert(dms_token = dms_token)

      tsui::pop_notice("上传成功")


    }


  })



}



#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' salOrderViewServer()
salOrderViewServer <- function(input,output,session,dms_token) {

  #获取参数
  text_salOredr_daterange = tsui::var_dateRange('text_salOredr_daterange')

  shiny::observeEvent(input$btn_salOrder_view,{

    FDate = text_salOredr_daterange()

    FStartDate = FDate[1]

    FEndDate = FDate[2]

    data = mdlDFSalOrderUploadPkg::salOrder_select(dms_token = dms_token,FStartDate =FStartDate ,FEndDate = FEndDate)


    tsui::run_dataTable2(id = 'salOrder_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_salOrder',data = data,filename = 'BYD销售订单.xlsx')




  })



}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' salOrderServer()
salOrderServer <- function(input,output,session,dms_token) {

  salOrderUploadServer(input = input,output = output,session = session,dms_token = dms_token)



  salOrderViewServer(input = input,output = output,session = session,dms_token = dms_token)


}
