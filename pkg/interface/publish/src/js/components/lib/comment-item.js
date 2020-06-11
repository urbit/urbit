import React, { Component } from 'react';
import moment from 'moment';
import { Sigil } from './icons/sigil';
import { CommentInput } from './comment-input';
import { uxToHex, cite } from '../../lib/util';

export class CommentItem extends Component {
  constructor(props) {
    super(props);

    this.state = {
      commentBody: ''
    };

    this.commentChange = this.commentChange.bind(this);
    this.commentEdit = this.commentEdit.bind(this);
    moment.updateLocale('en', {
      relativeTime: {
        past: function(input) {
          return input === 'just now'
            ? input
            : input + ' ago';
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
        yy : '%d years'
      }
    });
  }

  commentEdit() {
    const commentPath = Object.keys(this.props.comment)[0];
    const commentBody = this.props.comment[commentPath].content;
    this.setState({ commentBody });
    this.props.onEdit();
  }

  focusTextArea(text) {
    text && text.focus();
  }

  commentChange(e) {
    this.setState({
      commentBody: e.target.value
    });
  }

  onUpdate() {
    this.props.onUpdate(this.state.commentBody);
  }

  render() {
    const pending = this.props.pending ? 'o-60' : '';
    const commentData = this.props.comment[Object.keys(this.props.comment)[0]];
    const content = commentData.content.split('\n').map((line, i) => {
      return (
        <p className="mb2" key={i}>{line}</p>
      );
    });
    const date = moment(commentData['date-created']).fromNow();

    const contact = commentData.author.substr(1) in this.props.contacts
      ? this.props.contacts[commentData.author.substr(1)] : false;

    let name = commentData.author;
    let color = '#000000';
    let classes = 'mix-blend-diff';
    let avatar = null;
    if (contact) {
      name = (contact.nickname.length > 0)
        ? contact.nickname : commentData.author;
      color = `#${uxToHex(contact.color)}`;
      classes = '';
      avatar = contact.avatar;
    }

    const img = (avatar !== null)
      ? <img src={avatar} height={24} width={24} className="dib" />
      : <Sigil
        ship={commentData.author}
        size={24}
        color={color}
        classes={classes}
        />;

    if (name === commentData.author) {
      name = cite(commentData.author);
    }

    const { editing } = this.props;

    const disabled = this.props.pending
          ||  window.ship !== commentData.author.slice(1);

    return (
      <div className={'mb8 ' + pending}>
        <div className="flex mv3 bg-white bg-gray0-d">
        {img}
          <div className={'f9 mh2 pt1 ' +
            (contact.nickname ? null : 'mono')}
            title={commentData.author}
          >
            {name}
          </div>
          <div className="f9 gray3 pt1">{date}</div>
          { !editing && !disabled && (
            <>
              <div onClick={this.commentEdit.bind(this)} className="green2 pointer ml2 f9 pt1">
                Edit
              </div>
              <div onClick={this.props.onDelete} className="red2 pointer ml2 f9 pt1">
                Delete
              </div>
            </>
          ) }
        </div>
        <div className="f8 lh-solid mb2">
          { !editing && content }
          { editing && (
            <CommentInput style={{ resize:'vertical' }}
              ref={(el) => {
              this.focusTextArea(el);
              }}
              onChange={this.commentChange}
              value={this.state.commentBody}
              onSubmit={this.onUpdate.bind(this)}
            >
            </CommentInput>
          )}
        </div>
        { editing && (
          <div className="flex">
            <div onClick={this.onUpdate.bind(this)} className="br1 green2 pointer f9 pt1 b--green2 ba pa2 dib">
              Submit
            </div>
            <div onClick={this.props.onEditCancel} className="br1 black white-d pointer f9 b--gray2 ba pa2 dib ml2">
              Cancel
            </div>
          </div>
        )}
      </div>
    );
  }
}

export default CommentItem;
