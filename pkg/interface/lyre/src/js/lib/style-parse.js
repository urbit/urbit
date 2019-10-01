export function parseAllStyles(style) {
  let result = style.reduce((acc, el) => {
    let res = parseFlex(acc, el);
    if (res === acc) res = parseLayout(acc, el);
    if (res === acc) res = parseTypography(acc, el);
    if (res === acc) res = parseBGColor(acc, el);
    if (res === acc) res = parseBorder(acc, el);
    if (res === acc) res = parseSpace(acc, el);
    return res;
  }, {});
  return result;
}

export function parseFlex(res, el) {
  switch (el.property) {
    case "axis":
      res["display"] = "flex";
      res["flexDirection"] = (el.value === "row")
        ? "row" : "column";
      return res;
    case "basis":
      res["display"] = "flex";
      res["flexBasis"] = el.value;
      return res;
    case "grow":
      res["display"] = "flex";
      res["flexGrow"] = el.value;
      return res;
    case "shrink":
      res["display"] = "flex";
      res["flexShrink"] = el.value;
      return res;
    default:
      return res;
  }
}

export function parseLayout(res, el) {
  switch (el.property) {
    case "width":
      res["width"] = el.value;
      return res;
    case "height":
      res["height"] = el.value;
      return res;
    default:
      return res;
  }
}

export function parseTypography(res, el) {
  if (!res["lineHeight"]){
    res["lineHeight"] = 1.33333;
  }
  switch (el.property) {
    case "color":
      res["color"] = el.value;
      return res;
    case "font-family":
      res["fontFamily"] = el.value;
      return res;
    case "font-size":
      res["fontSize"] = el.value;
      return res;
    case "font-weight":
      res["fontWeight"] = el.value;
      return res;
    default:
      return res;
  }
}

export function parseBGColor(res, el) {
  switch (el.property) {
    case "bg-color":
      res["backgroundColor"] = el.value;
      return res;
    default:
      return res;
  }
}

export function parseBorder(res, el) {
  switch (el.property) {
    case "border":
      res["border"] = (el.value)
        ? "1px solid black" : "none";
      return res;
    case "ba":
      res["border"] = (el.value)
        ? "1px solid black" : "none";
      return res;
    case "by":
      res["borderTop"] = (el.value)
        ? "1px solid black" : "none";
      res["borderBottom"] = (el.value)
        ? "1px solid black" : "none";
      return res;
    case "bx":
      res["borderLeft"] = (el.value)
        ? "1px solid black" : "none";
      res["borderRight"] = (el.value)
        ? "1px solid black" : "none";
      return res;
    case "bt":
      res["borderTop"] = (el.value)
        ? "1px solid black" : "none";
      return res;
    case "bb":
      res["borderBottom"] = (el.value)
        ? "1px solid black" : "none";
      return res;
    case "bl":
      res["borderLeft"] = (el.value)
        ? "1px solid black" : "none";
      return res;
    case "br":
      res["borderRight"] = (el.value)
        ? "1px solid black" : "none";
      return res;
    default:
      return res;
  }
}

export function parseSpace(res, el) {
  switch (el.property) {
    case "m":
      res["margin"] = el.value;
      return res;
    case "my":
      res["marginTop"] = el.value;
      res["marginBottom"] = el.value;
      return res;
    case "mx":
      res["marginLeft"] = el.value;
      res["marginRight"] = el.value;
      return res;
    case "mt":
      res["marginTop"] = el.value;
      return res;
    case "mb":
      res["marginBottom"] = el.value;
      return res;
    case "ml":
      res["marginLeft"] = el.value;
      return res;
    case "mr":
      res["marginRight"] = el.value;
      return res;
    default:
      return res;
  }
}


