import React, { Component } from 'react';
import classnames from 'classnames';
import { TitleSnippet } from '/components/lib/title-snippet';
import { PostSnippet } from '/components/lib/post-snippet';
import { Link } from 'react-router-dom';
import moment from 'moment';

class Preview extends Component {
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

  buildProps(postId){
    let post = this.props.blog.posts[postId];
    return {
      postTitle: post.post.info.title,
      postName: post.post.info.filename,
      postBody: post.post.body,
      numComments: post.comments.length,
      collectionTitle: this.props.blog.info.title,
      collectionName: this.props.blog.info.filename,
      author: post.post.info.creator,
      blogOwner: this.props.blog.info.owner,
      date: post.post.info["date-created"],
      pinned: false,
    }
  }

  render(){
    if (this.props.postId) {
      let owner = this.props.blog.info.owner;
      let blogId = this.props.blog.info.filename;
      let previewProps = this.buildProps(this.props.postId);
      let prevUrl = `/~publish/${owner}/${blogId}/${this.props.postId}`

      let date = moment(previewProps.date).fromNow();
      let authorDate = `${previewProps.author} • ${date}`
      let collLink = "/~publish/" + 
        previewProps.blogOwner + "/" +
        previewProps.collectionName;
      let postLink = collLink + "/" + previewProps.postName;

      return (
        <div className="w-336">
          <Link className="ml2 mr2 gray-50 body-regular" to={prevUrl}>
            {this.props.text}
          </Link>
          <div className="w-336 relative"
            style={{height:195}}>
            <Link to={postLink}>
              <TitleSnippet title={previewProps.postTitle}/>
              <PostSnippet
                body={previewProps.postBody}
              />
            </Link>
            <p className="label-small gray-50 absolute" style={{bottom:0}}>
              {authorDate}
            </p>
          </div>
        </div>
      );
    } else {
      return (
        <div className="w-336"></div>
      );
    }
  }
}

export class NextPrev extends Component {
  constructor(props) {
    super(props);
  }



  render() {
    let posts = this.props.blog.order.unpin.slice().reverse();
    let postIdx = posts.indexOf(this.props.postId);

    let prevId = (postIdx > 0)
      ?  posts[postIdx - 1]
      :  false;

    let nextId = (postIdx < (posts.length - 1))
      ?  posts[postIdx + 1]
      :  false;

    if (!(prevId || nextId)){
      return null;
    } else {
      let prevText = "<- Previous Post";
      let nextText = "-> Next Post";

      return (
        <div>
          <div className="flex">
            <Preview postId={prevId} blog={this.props.blog} text={prevText}/>
            <div style={{width:16}}></div>
            <Preview postId={nextId} blog={this.props.blog} text={nextText}/>
          </div>
          <hr className="gray-50 w-680 mt4"/>
        </div>
      );
    }
  }
}
