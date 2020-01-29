import React, { Component } from 'react'
import { CommentItem } from './comment-item';

//TODO map comments into comment-items;
export class Comments extends Component {
  render() {
    return (
      <div>
        <form class="mt8">
          <div>
            <textarea style={{resize:'vertical'}} id="comment" name="comment" placeHolder="Leave a comment here" class="f9 db border-box w-100 ba b--gray3 pt3 ph3 pb8 br1 mb2" aria-describedby="comment-desc"></textarea>
          </div>
          <button className="f9 pa2 bg-white br1 ba b--gray2 gray2">
            Add comment
          </button>
        </form>
        <CommentItem/>
      </div>
    )
  }
}

export default Comments
