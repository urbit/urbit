import React, { Component } from 'react'
import { CommentItem } from './comment-item';
import { dateToDa } from '/lib/util';
import { Spinner } from './icons/icon-spinner';

export class Comments extends Component {
  constructor(props){
    super(props);
    this.state = {
      commentBody: '',
      disabled: false,
      pending: new Set()
    }
    this.commentSubmit = this.commentSubmit.bind(this);
    this.commentChange = this.commentChange.bind(this);
  }

  componentDidUpdate(prevProps) {
    let previousComments = prevProps.comments[0] || {};
    let currentComments = this.props.comments[0] || {};
    let previous = Object.keys(previousComments) || [];
    let current = Object.keys(currentComments) || [];
    if ((prevProps.comments && this.props.comments) &&
      (previous !== current)) {
        let pendingSet = this.state.pending;
        Object.keys(currentComments).map((com) => {
          let obj = currentComments[com];
          for (let each of pendingSet.values()) {
            if (obj.content === each["new-comment"].body) {
              pendingSet.delete(each);
              this.setState({ pending: pendingSet });
            }
          }
        })
      }
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

   let pendingState = this.state.pending;
   pendingState.add(comment);
   this.setState({pending: pendingState});

   this.textArea.value = '';
   this.setState({commentBody: "", disabled: true});
   let submit = window.api.action("publish", "publish-action", comment);
   submit.then(() => {
     this.setState({ disabled: false });
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

    let pendingArray = Array.from(this.state.pending).map((com, i) => {
      let da = dateToDa(new Date);
      let comment = {
        [da]: {
          author: `~${window.ship}`,
          content: com["new-comment"].body,
          "date-created": Math.round(new Date().getTime())
        }
      }
      return (
        <CommentItem
          comment={comment}
          key={i}
          contacts={this.props.contacts}
          pending={true}
        />
      )
    });

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
      ?  "bg-transparent f9 pa2 br1 ba b--gray2 gray2"
      :  "bg-transparent f9 pa2 br1 ba b--gray2 black white-d pointer";

    return (
      <div>
        <div className="mv8 relative">
          <div>
            <textarea style={{resize:'vertical'}}
              ref={(el) => {this.textArea = el}}
              id="comment"
              name="comment"
              placeholder="Leave a comment here"
              className={"f9 db border-box w-100 ba b--gray3 pt3 ph3 br1 " +
              "b--gray2-d mb2 focus-b--black focus-b--white-d white-d bg-gray0-d"}
              aria-describedby="comment-desc"
              style={{height: "4rem"}}
              onChange={this.commentChange}
              onKeyDown={(e) => {
                if ((e.getModifierState("Control") || event.metaKey)
                  && e.key === "Enter") {
                    this.commentSubmit();
                  }
              }}>
            </textarea>
          </div>
          <button disabled={disableComment}
            onClick={this.commentSubmit}
            className={commentClass}>
            Add comment
          </button>
          <Spinner text="Posting comment..." awaiting={this.state.disabled} classes="absolute bottom-0 right-0 pb2"/>
        </div>
        {pendingArray}
        {commentArray}
      </div>
    )
  }
}

export default Comments
