import React, { Component } from 'react';
import classnames from 'classnames';
import { Comment } from '/components/lib/comment';
import { CommentBox } from '/components/lib/comment-box';
import { Sigil } from '/components/lib/icons/sigil';

export class Comments extends Component {
  constructor(props){
    super(props);

    this.state = {
      show: false,
      commentBody: '',
      awaiting: false,
    }
    
    this.toggleDisplay = this.toggleDisplay.bind(this);
    this.commentChange = this.commentChange.bind(this);
    this.postComment   = this.postComment.bind(this);
  }

  commentChange(evt) {
    this.setState({commentBody: evt.target.value});
  }

  toggleDisplay() {
    this.setState({show: !this.state.show});
  }

  postComment() {
    let comment = {
      "new-comment": {
        who: this.props.ship,
        coll: this.props.blogId,
        name: this.props.postId,
        content: this.state.commentBody,
      }
    };

    this.setState({
      awaiting: {
        ship: this.props.ship,
        blogId: this.props.blogId,
        postId: this.props.postId,
      }
    });

    this.props.api.action("write", "write-action", comment);
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.state.awaiting) {
      if (prevProps.comments != this.props.comments) {
        this.setState({awaiting: false, commentBody: ''});
      }
    }
  }

  render(){
    if (this.state.show) {
      let our = `~${window.ship}`;
      let comments = this.props.comments.map((comment, i) => {
        let commentProps = {
          ship: comment.info.creator,
          date: comment.info["date-created"],
          body: comment.body,
        };
        return (<Comment {...commentProps} key={i} />);
      });
      return (
        <div className="cb mt3 mb4">
          <p className="gray-50 body-large b">
            {this.props.comments.length}
            <span className="black">
              Comments
            </span>
          </p>
          <p className="cl body-regular pointer" onClick={this.toggleDisplay}>
            - Hide Comments
          </p>

          <CommentBox our={our}
            action={this.commentChange}
            enabled={!(Boolean(this.state.awaiting))}
            content={this.state.commentBody}
            post={this.postComment}/>


          <div className="flex-col" style={{marginTop: 32}}>
            {comments}
          </div>
        </div>
      );
    } else {
      return (
        <div className="cb mt3 mb4">
          <p className="gray-50 body-large b">
            {this.props.comments.length}
            <span className="black">
              Comments
            </span>
          </p>
          <p className="cl body-regular pointer" onClick={this.toggleDisplay}>
            + Show Comments
          </p>
        </div>
      );
    }
  }
}
