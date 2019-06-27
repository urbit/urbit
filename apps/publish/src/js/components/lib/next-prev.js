import React, { Component } from 'react';
import classnames from 'classnames';
import { PostPreview } from '/components/post-preview';
import { Link } from 'react-router-dom';

class Preview extends Component {
  constructor(props){
    super(props);
  }

  buildProps(postId){
    let post = this.props.blog.posts[postId];
    console.log("blog", this.props.blog);
    console.log("post", postId, post);
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
      return (
        <div className="w-336">
          <Link className="ml2 mr2 gray-50 body-regular" to={prevUrl}>
            {this.props.text}
          </Link>
          <PostPreview post={previewProps} />
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
    console.log(this.props);

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
            <Preview postId={nextId} blog={this.props.blog} text={nextText}/>
          </div>
          <hr className="gray-50 w-680 mt4"/>
        </div>
      );
    }
  }
}
