import React, { Component } from 'react'

//TODO take props and render div
export class CommentItem extends Component {
  render() {
    return (
      <div>
        <div className="flex mv3">
          <div className="f9 mono mr2">~fabled-faster</div>
          <div className="f9 gray3">12m ago</div>
        </div>
        <div className="f8 lh-solid mb8">
          <p className="mb2">
            Wow, impressive results! Already a few examples in the comments of what bad actors could do this tech. I wanted to share an example of something good.
          </p>

          <p className="mb2">
            I lost my dad about 6 years ago after a Stage 4 cancer diagnosis and a 3 month rapid diagnosis. I have some, but not a lot of video content of him from over the years. My mom still misses him terribly so for her 60th birthday I tried to splice together an audio message and greeting from her saying what I thought he would have said.
          </p>
          <p className="mb2">
          The work was rough and nowhere near what this Google project could produce. She listens to that poor facsimile every year for her birthday. It's therapeutic for her. With some limits for her mental health of course, I'm sure she would love to hear my dad again with this level of fidelity.
          And so would I.
          </p>
        </div>
      </div>
    )
  }
}

export default CommentItem
