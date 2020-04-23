import React, { Component } from 'react'
import { CommentItem } from './comment-item';
import { CommentInput } from './comment-input';
import { dateToDa } from '../../lib/util';
import { Spinner } from './icons/icon-spinner';

export class Comments extends Component {
  constructor(props){
    super(props);
    this.state = {
      commentBody: '',
      pending: new Set(),
      awaiting: null,
      editing: null,
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
   this.setState({commentBody: "", awaiting: 'new'});
   let submit = window.api.action("publish", "publish-action", comment);
   submit.then(() => {
     this.setState({ awaiting: null });
    })
   }

  commentChange(evt){
    this.setState({
      commentBody: evt.target.value,
    })
  }

  commentEdit(idx) {
    this.setState({ editing: idx });
  }

  commentEditCancel() {
    this.setState({ editing: null });
  }


  commentUpdate(idx, body) {

    let path = Object.keys(this.props.comments[idx])[0];
    let comment = {
      "edit-comment": {
        who: this.props.ship.slice(1),
        book: this.props.book,
        note: this.props.note,
        body: body,
        comment: path
      }
    };

    this.setState({ awaiting: 'edit' })

    window.api
      .action('publish', 'publish-action', comment)
      .then(() => { this.setState({ awaiting: null, editing: null }) })
  }

  commentDelete(idx) {
    let path = Object.keys(this.props.comments[idx])[0];
    let comment = {
      "del-comment": {
        who: this.props.ship.slice(1),
        book: this.props.book,
        note: this.props.note,
        comment: path
      }
    };

    this.setState({ awaiting: { kind: 'del', what: idx }})
    window.api
      .action('publish', 'publish-action', comment)
      .then(() => { this.setState({ awaiting: null }) })

  }

  render() {
    if (!this.props.enabled) {
      return null;
    }

    const { editing } = this.state;

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
          onUpdate={u => this.commentUpdate(i, u)}
          onDelete={() => this.commentDelete(i)}
          onEdit={() => this.commentEdit(i)}
          onEditCancel={this.commentEditCancel.bind(this)}
          editing={i === editing}
          disabled={!!this.state.awaiting || editing}
          />
      );
    })

    let disableComment = ((this.state.commentBody === '') || (!!this.state.awaiting));
    let commentClass = (disableComment)
      ?  "bg-transparent f9 pa2 br1 ba b--gray2 gray2"
      :  "bg-transparent f9 pa2 br1 ba b--gray2 black white-d pointer";

    let spinnerText =
        this.state.awaiting === 'new'
        ? 'Posting commment...'
        : this.state.awaiting  === 'edit'
        ? 'Updating comment...'
        : 'Deleting comment...';

    return (
      <div>
        <div className="mv8 relative">
          <div>
            <CommentInput style={{resize:'vertical'}}
              ref={(el) => {this.textArea = el}}
              onChange={this.commentChange}
              value={this.state.commentBody}
              disabled={!!this.state.editing}
              onSubmit={this.commentSubmit}>
            </CommentInput>
          </div>
          <button disabled={disableComment}
            onClick={this.commentSubmit}
            className={commentClass}>
            Add comment
          </button>
          <Spinner text={spinnerText} awaiting={this.state.awaiting} classes="absolute bottom-0 right-0 pb2"/>
        </div>
        {pendingArray}
        {commentArray}
      </div>
    )
  }
}

export default Comments
