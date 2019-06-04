import React, { Component } from 'react';
import classnames from 'classnames';
import { PostPreview } from '/components/post-preview';
import moment from 'moment';
import { Link } from 'react-router-dom';


export class PostSnippet extends Component {
  constructor(props){
    super(props)
  }

  render() {
    let elem = this.props.body.c.find((elem) => {
      return elem.gn === "p" &&
        typeof(elem.c[0]) === "string";
    });

    let string = elem.c[0];
    let words = string.split(" ");

    let snip = new String(string);

    if (words.length > 0 && words[0].length > 280) {
      snip = words[0].slice(0, 280).trim() + " [...]";
    } else if (snip.length > 280){
      while (snip.length > 280) {
        snip = snip.split(" ").slice(0, -1).join(" ");
      }
      snip += " [...]";
    }


    return (
      <p className="body-regular-400">
        {snip}
      </p>
    );
  }
}

