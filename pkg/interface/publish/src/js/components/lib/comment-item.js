import React, { Component } from 'react';
import moment from 'moment';

//TODO take props and render div
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


    return (
      <div>
        <div className="flex mv3">
          <div className="f9 mono mr2">{commentData.author}</div>
          <div className="f9 gray3">{date}</div>
        </div>
        <div className="f8 lh-solid mb8 mb2">
          {content}
        </div>
      </div>
    )
  }
}

export default CommentItem
