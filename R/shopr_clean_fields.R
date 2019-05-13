shopr_clean_fields <- function(fields, resource){
  # Validate fields
  # Return NULL or a length-1 character of comma separated feilds
  # resource can be one of {'orders', 'products'}

  fieldsPossible <- if(resource == "orders"){
    c(
      "id", "email", "closed_at", "created_at", "updated_at", "number",
      "note", "token", "gateway", "test", "total_price", "subtotal_price",
      "total_weight", "total_tax", "taxes_included", "currency", "financial_status",
      "confirmed", "total_discounts", "total_line_items_price", "cart_token",
      "buyer_accepts_marketing", "name", "referring_site", "landing_site",
      "cancelled_at", "cancel_reason", "total_price_usd", "checkout_token",
      "reference", "user_id", "location_id", "source_identifier", "source_url",
      "processed_at", "device_id", "phone", "customer_locale", "app_id",
      "browser_ip", "landing_site_ref", "order_number", "discount_applications",
      "discount_codes", "note_attributes", "payment_gateway_names",
      "processing_method", "checkout_id", "source_name", "fulfillment_status",
      "tax_lines", "tags", "contact_email", "order_status_url", "presentment_currency",
      "total_line_items_price_set", "total_discounts_set", "total_shipping_price_set",
      "subtotal_price_set", "total_price_set", "total_tax_set", "total_tip_received",
      "admin_graphql_api_id", "line_items", "shipping_lines", "billing_address",
      "shipping_address", "fulfillments", "client_details", "refunds",
      "payment_details", "customer"
    )
  } else if(resource == "products"){
    c(
      "id", "title", "body_html", "vendor", "product_type", "created_at",
      "handle", "updated_at", "published_at", "template_suffix", "tags",
      "published_scope", "admin_graphql_api_id", "variants", "options",
      "images", "image.id", "image.product_id", "image.position", "image.created_at",
      "image.updated_at", "image.alt", "image.width", "image.height",
      "image.src", "image.variant_ids", "image.admin_graphql_api_id"
    )
  }

  if(is.null(fields) || length(fields) == 0L){
    return(NULL)
  } else if(!is.character(fields)){
    stop("'fields' should be type character")
  } else{
    if(length(fields) == 1 && stringr::str_detect(fields, ",")){
      # fields is a comma-separated string. transform it into a vector of individual fields
      fieldsVec <- stringr::str_split(string = fields, pattern = ",")
    } else{
      fieldsVec <- fields
    }

    # force id field to be included
    if(!"id" %in% fieldsVec) fieldsVec <- c('id', fieldsVec)

    # Check for unknown fields
    fieldsUnknown <- setdiff(fieldsVec, fieldsPossible)
    if(length(fieldsUnknown) > 0){
      stop(paste0('One or more fields not recognized: ', paste(fieldsVec, collapse = ",")))
    }

    # Convert to comma-separated string
    fieldsStr <- paste(fields, collapse = ",")
    return(fieldsStr)
  }
}
