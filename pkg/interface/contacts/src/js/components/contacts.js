import React, { Component } from 'react'

export class Contacts extends Component {

  constructor(props) {
    super(props);
    this.api = props.api;
    this.readFile = this.readFile.bind(this);
  }

    readFile(e) {
      let reader = new FileReader();

      reader.onload = () => {
        let arr = reader.result.split(';base64,');
        let type = arr[0].split('data:')[1];
        let data = arr[1];
        this.api.contacts.edit('/~/default', '~zod', {
          avatar: {
            'content-type': type,
            octs: {
              p: data.length,
              q: data
            }
          }
        });
      };

      reader.readAsDataURL(e.target.files[0]);
    }

    render() {
        return (
            <div class="br b--gray4 h-100 flex-shrink-0 flex-basis-100-s flex-basis-300-ns relative">
              <p>Hey.</p>
              <input
                id="upload" ref="upload" type="file" accept="image/*"
                onChange={(event)=> { 
                  this.readFile(event) 
                }}
                onClick={(event)=> { 
                  event.target.value = null
                }} />
            </div>
        )
    }
}

export default Contacts
