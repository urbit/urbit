import React, { Component } from 'react';
import classnames from 'classnames';
import moment from 'moment';
import { Link } from 'react-router-dom';
import { PostSnippet } from '/components/lib/post-snippet';
import { TitleSnippet } from '/components/lib/title-snippet';

export class PostPreview extends Component {
  constructor(props) {
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
    let comments = this.props.post.numComments == 1
      ? '1 comment'
      : `${this.props.post.numComments} comments`;
    let date = moment(this.props.post.date).fromNow();
    let authorDate = `${this.props.post.author} • ${date}`
    let collLink = "/~publish/" +
      this.props.post.blogOwner + "/" +
      this.props.post.collectionName;
    let postLink = collLink + "/" + this.props.post.postName;

    return (
      <div className="w-336 relative"
        style={{height:195, marginBottom: 72, marginRight:16}}>
        <Link to={postLink}>
          <TitleSnippet badge={this.props.post.unread} title={this.props.post.postTitle}/>
          <PostSnippet
            body={this.props.post.postBody}
          />
        </Link>
        <p className="label-small gray-50 absolute" style={{bottom:0}}>
          {authorDate}
        </p>
      </div>
    );
  }
}
