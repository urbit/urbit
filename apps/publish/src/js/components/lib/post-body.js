import React, { Component } from 'react';
import classnames from 'classnames';
import moment from 'moment';
import { Link } from 'react-router-dom';


export class PostBody extends Component {
  constructor(props){
    super(props)
  }

  renderA(what, node, attr) {
    let aStyle = {
      textDecorationLine: "underline",
      wordWrap: "break-word"
    };
    let children = what.map((item, key) => {
      if (typeof(item) === 'string') {
        return item;
      } else {
        let newAttr = Object.assign({style: aStyle, key: key}, item.ga);
        return this.parseContent(item.c, item.gn, newAttr);
      }
    });
    const element =
      React.createElement(node, Object.assign({style: aStyle}, attr), children);
    return element;
  }


  renderIMG(what, node, attr) {
    let imgStyle = {
      width: "100%",
      height: "auto"
    };
    let newAttr = Object.assign({style: imgStyle}, attr);
    const element = React.createElement(node, newAttr);
    return element;
  }

  renderDefault(what, node, attr) {
    let dStyle = {
      wordWrap: "break-word"
    };
    let children = what.map((item, key) => {
      if (typeof(item) === 'string') {
        return item;
      } else {
        let newAttr = Object.assign({key: key}, item.ga);
        return this.parseContent(item.c, item.gn, newAttr);
      }
    });
    const element =
      React.createElement(node, Object.assign({style: dStyle}, attr), children);
    return element;
  }

  parseContent(what, node, attr) {
    switch (node) {
      case "a":
        return this.renderA(what, node, attr);
      case "img":
        return this.renderIMG(what, node, attr);
      default:
        return this.renderDefault(what, node, attr);
    }
  }


  render() {
    let page = this.parseContent(this.props.body.c, this.props.body.gn, this.props.body.ga); 
    return page;
  }
}

