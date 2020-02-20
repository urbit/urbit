import React, { Component } from 'react';
import classnames from 'classnames';
import moment from 'moment';
import { Link } from 'react-router-dom';


export class PostBody extends Component {
  constructor(props){
    super(props)
  }

  renderA(what, node, attr, parentNode) {
    let aStyle = {
      textDecorationLine: "underline",
      wordWrap: "break-word"
    };
    let children = what.map((item, key) => {
      if (typeof(item) === 'string') {
        return item;
      } else {
        let newAttr = Object.assign({style: aStyle, key: key}, item.ga);
        return this.parseContent(item.c, item.gn, newAttr, node);
      }
    });
    const element =
      React.createElement(node, Object.assign({style: aStyle}, attr), children);
    return element;
  }


  renderIMG(what, node, attr, parentNode) {
    let imgStyle = {
      width: "100%",
      height: "auto",
      marginBottom: 12,
    };
    let newAttr = Object.assign({style: imgStyle}, attr);
    const element = React.createElement(node, newAttr);
    return element;
  }

  renderP(what, node, attr, parentNode) {
    let dStyle = {
      wordWrap: "break-word",
    };
    if (parentNode !== 'li') {
      dStyle.marginBottom = 12;
    }
    let children = what.map((item, key) => {
      if (typeof(item) === 'string') {
        return item;
      } else {
        let newAttr = Object.assign({key: key}, item.ga);
        return this.parseContent(item.c, item.gn, newAttr, node);
      }
    });
    const element =
      React.createElement(node, Object.assign({style: dStyle}, attr), children);
    return element;
  }

  renderHR(what, node, attr, parentNode) {
    const element = React.createElement(node, attr);
    return element;
  }

  renderDefault(what, node, attr, parentNode) {
    let dStyle = {
      wordWrap: "break-word",
    };
    let children = what.map((item, key) => {
      if (typeof(item) === 'string') {
        return item;
      } else {
        let newAttr = Object.assign({key: key}, item.ga);
        return this.parseContent(item.c, item.gn, newAttr, node);
      }
    });
    const element =
      React.createElement(node, Object.assign({style: dStyle}, attr), children);
    return element;
  }

  parseContent(what, node, attr, parentNode) {
    switch (node) {
      case "a":
        return this.renderA(what, node, attr, parentNode);
      case "img":
        return this.renderIMG(what, node, attr, parentNode);
      case "p":
        return this.renderP(what, node, attr, parentNode);
      case "hr":
        return this.renderHR(what, node, attr, parentNode);
      default:
        return this.renderDefault(what, node, attr, parentNode);
    }
  }


  render() {
    let page = this.parseContent(this.props.body.c,
      this.props.body.gn,
      this.props.body.ga,
      null);
    return page;
  }
}

