import React, { Component } from 'react';
import { Comments } from './comments';
import moment from 'moment';
import ReactMarkdown from 'react-markdown'
// import test from 'test.json';
//TODO ask for note if we don't have it
//TODO initialise note if no state

//TODO if comments are disabled on the notebook, don't render comments
export class Note extends Component {
  constructor(props){
    super(props);
    console.log(this.props);
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
    let comments = this.props.notebook.notes[this.props.note].comments;
    let title = this.props.notebook.notes[this.props.note].title;
    let author = this.props.notebook.notes[this.props.note].author;
    let file = this.props.notebook.notes[this.props.note].file;
    let date = moment(this.props.notebook.notes[this.props.note]["date-created"]).fromNow();

    let newfile = file.slice(file.indexOf(';>')+2);


    return (
      <div className="h-100 overflow-container">
        <div className="flex justify-center mt4 ph4 pb4">
          <div className="w-100 mw6">
            <div className="flex flex-column">
              <div className="f9 mb1">{title}</div>
              <div className="flex">
              <div className="di f9 mono gray2 mr2">{author}</div>
              <div className="di f9 gray2">{date}</div>
              </div>
            </div>

            <ReactMarkdown source={newfile} />

            <div className="flex mt4">
              <a href="" className="di flex-column w-50 pv6 bt br bb b--gray3">
                <div className="f9 gray2 mb2">Previous</div>
                <div className="f9 mb1">%loud</div>
                <div className="f9 gray2">14d ago</div>
              </span>
              <a href="" className="di flex-column tr w-50 pv6 bt bb b--gray3">
              <div className="f9 gray2 mb2">Next</div>
              <div className="f9 mb1">+advent of %code ~2019.12</div>
              <div className="f9 gray2">6d ago</div>
              </div>
            </div>
            <Comments ship={this.props.ship} book={this.props.book} note={this.props.note} comments={comments}/>

          </div>
        </div>
      </div>
    )
  }
}

export default Note
