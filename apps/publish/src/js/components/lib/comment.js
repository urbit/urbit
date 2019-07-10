import React, { Component } from 'react';
import classnames from 'classnames';
import { Sigil } from '/components/lib/icons/sigil';
import moment from 'moment';

export class Comment extends Component {
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

  render(){
    let body = this.props.body.split("\n").map((line, i) =>{
      return (<p key={i}>{line}</p>);
    });

    let date = moment(this.props.date).fromNow();

    return (
      <div className="cb w-100 flex" style={{paddingBottom: 16}}>
        <div className="fl" style={{marginRight: 10}}>
          <Sigil ship={this.props.ship} size={36} />
        </div>
        <div className="flex-col fl">
          <div className="label-small-mono gray-50">
            <p className="fl" style={{width: 107}}>{this.props.ship}</p>
            <p className="fl">{date}</p>
          </div>
          <div className="cb body-regular-400">
            {body}
          </div>
        </div>
      </div>
    );
  }
}
