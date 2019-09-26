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
    default:
      return res;
  }
}
