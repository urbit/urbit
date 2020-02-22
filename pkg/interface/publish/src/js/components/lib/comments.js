import React, { Component } from 'react'
import { CommentItem } from './comment-item';

export class Comments extends Component {
  constructor(props){
    super(props);
    this.state = {
      commentBody: '',
      disabled: false
    }
    this.commentSubmit = this.commentSubmit.bind(this);
    this.commentChange = this.commentChange.bind(this);
  }
 commentSubmit(evt){
   let comment = {
     "new-comment": {
       who: this.props.ship.slice(1),
       book: this.props.book,
       note: this.props.note,
       body: this.state.commentBody
     }
   };

   this.textArea.value = '';
   window.api.setSpinner(true);
   this.setState({disabled: true});
   let submit = window.api.action("publish", "publish-action", comment);
   submit.then(() => {
     window.api.setSpinner(false);
     this.setState({ disabled: false, commentBody: "" });
    })
   }

  commentChange(evt){
    this.setState({
      commentBody: evt.target.value,
    })
  }

  render() {
    if (!this.props.enabled) {
      return null;
    }
    let commentArray = this.props.comments.map((com, i) => {
      return (
        <CommentItem
          comment={com}
          key={i}
          contacts={this.props.contacts}
          />
      );
    })

    let disableComment = ((this.state.commentBody === '') || (this.state.disabled === true));
    let commentClass = (disableComment)
      ?  "f9 pa2 bg-white br1 ba b--gray2 gray2"
      :  "f9 pa2 bg-white br1 ba b--gray2 black pointer";

    return (
      <div>
        <div className="mt8">
          <div>
            <textarea style={{resize:'vertical'}}
              ref={(el) => {this.textArea = el}}
              id="comment"
              name="comment"
              placeholder="Leave a comment here"
              className={"f9 db border-box w-100 ba b--gray3 pt3 ph3 pb8 br1 " +
              "mb2 focus-b--black"}
              aria-describedby="comment-desc"
              onChange={this.commentChange}>
            </textarea>
          </div>
          <button disabled={disableComment}
            onClick={this.commentSubmit}
            className={commentClass}>
            Add comment
          </button>
        </div>
        {commentArray}
      </div>
    )
  }
}

export default Comments
