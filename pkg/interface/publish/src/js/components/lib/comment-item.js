import React, { Component } from 'react';
import moment from 'moment';
import { Sigil } from './icons/sigil';
import { uxToHex } from '../../lib/util';

export class CommentItem extends Component {
  constructor(props){
    super(props);
    moment.updateLocale('en', {
      relativeTime: {
        past: function(input) {
          return input === 'just now'
            ? input
            : input + ' ago'
        },
        s : 'just now',
        future : 'in %s',
        m  : '1m',
        mm : '%dm',
        h  : '1h',
        hh : '%dh',
        d  : '1d',
        dd : '%dd',
        M  : '1 month',
        MM : '%d months',
        y  : '1 year',
        yy : '%d years',
      }
    });
  }
  render() {
    let commentData = this.props.comment[Object.keys(this.props.comment)[0]];
    let content = commentData.content.split("\n").map((line, i)=> {
      return (
        <p className="mb2" key={i}>{line}</p>
      )
    });
    let date = moment(commentData["date-created"]).fromNow();

    let contact = !!(commentData.author.substr(1) in this.props.contacts)
      ? this.props.contacts[commentData.author.substr(1)] : false;

    let name = commentData.author;
    let color = "#000000";
    if (contact) {
      name = (contact.nickname.length > 0)
        ? contact.nickname : commentData.author;
      color = `#${uxToHex(contact.color)}`;
    }


    return (
      <div>
        <div className="flex mv3">
        <Sigil
          ship={commentData.author}
          size={24}
          color={color}
        />
          <div className={"f9 mh2 pt1" +
          ((name === commentData.author) ? " mono" : "")}>
            {name}
          </div>
          <div className="f9 gray3 pt1">{date}</div>
        </div>
        <div className="f8 lh-solid mb8 mb2">
          {content}
        </div>
      </div>
    )
  }
}

export default CommentItem
