/**
 * This configuration was generated using the CKEditor 5 Builder. You can modify it anytime using this link:
 * https://ckeditor.com/ckeditor-5/builder/#installation/NoVgNARATAdA7PCkDMAWAnKuAGAbHADnV1wNygEYQosCKN77UK5VLldiQCDskIApgDskFMMDGSwY7NIC6kXAIBmFKCD5ygA=
 */

import {
        InlineEditor,
        DecoupledEditor,
	Autosave,
	Essentials,
	Paragraph,
	Bold,
	Italic,
	Underline,
	ImageInsertViaUrl,
	ImageBlock,
	ImageToolbar,
	AutoImage,
	CloudServices,
	ImageUpload,
	ImageStyle,
	ImageCaption,
	ImageTextAlternative,
	Table,
	TableToolbar,
	List,
	Strikethrough,
	Code,
	Subscript,
	Superscript,
	Heading,
	Link,
	AutoLink,
	BlockQuote,
	HorizontalLine,
	CodeBlock,
	Indent,
	IndentBlock,
	Alignment,
	GeneralHtmlSupport,
	Style,
	TableCaption,
	ShowBlocks,
	HtmlComment,
	ImageUtils,
	ImageEditing,
	Autoformat,
	TextTransformation,
	PlainTableOutput,
	BalloonToolbar//,
	//BlockToolbar
} from 'ckeditor5';

/**
 * Create a free account with a trial: https://portal.ckeditor.com/checkout?plan=free
 */
const LICENSE_KEY = 'GPL'; // or <YOUR_LICENSE_KEY>.

const editorConfig = {
	toolbar: {
		items: [
			'undo',
			'redo',
			'|',
			'showBlocks',
			'|',
			'heading',
			//'style',
			'|',
			'bold',
			'italic',
			'underline',
			'strikethrough',
			'subscript',
			'superscript',
			'code',
			'|',
			'horizontalLine',
                        'link',
                        'insertImage',
			'insertTable',
			'blockQuote',
			'codeBlock',
			'|',
			'alignment',
			'|',
			'bulletedList',
			'numberedList',
			'outdent',
			'indent'
		],
		shouldNotGroupWhenFull: false
	},
	plugins: [
		Alignment,
		Autoformat,
		AutoImage,
		AutoLink,
		Autosave,
		BalloonToolbar,
		BlockQuote,
		//BlockToolbar,
		Bold,
		CloudServices,
		Code,
		CodeBlock,
		Essentials,
		GeneralHtmlSupport,
		Heading,
		HorizontalLine,
		HtmlComment,
		ImageBlock,
		ImageCaption,
		ImageEditing,
		ImageInsertViaUrl,
		ImageStyle,
		ImageTextAlternative,
		ImageToolbar,
		ImageUpload,
		ImageUtils,
		Indent,
		IndentBlock,
		Italic,
		Link,
		List,
		Paragraph,
		PlainTableOutput,
		ShowBlocks,
		Strikethrough,
		//Style,
		Subscript,
		Superscript,
		Table,
		TableCaption,
		TableToolbar,
		TextTransformation,
		Underline
	],
	balloonToolbar: ['bold', 'italic', '|', 'link', '|', 'bulletedList', 'numberedList'],
	//blockToolbar: ['bold', 'italic', '|', 'link', 'insertTable', '|', 'bulletedList', 'numberedList', 'outdent', 'indent'],
	heading: {
		options: [
			{
				model: 'paragraph',
				title: 'Paragraph',
				class: 'ck-heading_paragraph'
			},
			{
				model: 'heading1',
				view: 'h1',
				title: 'Heading 1',
				class: 'ck-heading_heading1'
			},
			{
				model: 'heading2',
				view: 'h2',
				title: 'Heading 2',
				class: 'ck-heading_heading2'
			},
			{
				model: 'heading3',
				view: 'h3',
				title: 'Heading 3',
				class: 'ck-heading_heading3'
			},
			{
				model: 'heading4',
				view: 'h4',
				title: 'Heading 4',
				class: 'ck-heading_heading4'
			},
			{
				model: 'heading5',
				view: 'h5',
				title: 'Heading 5',
				class: 'ck-heading_heading5'
			},
			{
				model: 'heading6',
				view: 'h6',
				title: 'Heading 6',
				class: 'ck-heading_heading6'
			}
		]
	},
	htmlSupport: {
		allow: [
			{
				name: /^.*$/,
				styles: true,
				attributes: true,
				classes: true
			}
		]
	},
	image: {
		toolbar: [
			'toggleImageCaption',
			'imageTextAlternative',
			'|',
			'imageStyle:alignBlockLeft',
			'imageStyle:block',
			'imageStyle:alignBlockRight'
		],
		styles: {
			options: ['alignBlockLeft', 'block', 'alignBlockRight']
		}
	},
	licenseKey: LICENSE_KEY,
	link: {
		addTargetToExternalLinks: false,
		defaultProtocol: 'https://',
		decorators: {
			toggleDownloadable: {
				mode: 'manual',
				label: 'Downloadable',
				attributes: {
					download: 'file'
				}
			}
		}
	},
	placeholder: 'Type or paste your content here!',
	style: {
		definitions: [
			{
				name: 'Article category',
				element: 'h3',
				classes: ['category']
			},
			{
				name: 'Title',
				element: 'h2',
				classes: ['document-title']
			},
			{
				name: 'Subtitle',
				element: 'h3',
				classes: ['document-subtitle']
			},
			{
				name: 'Info box',
				element: 'p',
				classes: ['info-box']
			},
			{
				name: 'CTA Link Primary',
				element: 'a',
				classes: ['button', 'button--green']
			},
			{
				name: 'CTA Link Secondary',
				element: 'a',
				classes: ['button', 'button--black']
			},
			{
				name: 'Marker',
				element: 'span',
				classes: ['marker']
			},
			{
				name: 'Spoiler',
				element: 'span',
				classes: ['spoiler']
			}
		]
	},
	table: {
		contentToolbar: ['tableColumn', 'tableRow', 'mergeTableCells']
	}
};

console.log("executing ckeditor5-main.js")


/*
  With some badgering, I mostly let claude.ai implement this. After reviewing the code and asking
  some questions, it produced this useful summary:

    The loader object is an instance of CKEditor 5's FileLoader class from the FileRepository plugin. Here's what it provides:

    Key properties:
    - file - A Promise that resolves to the native browser File object being uploaded
    - uploaded - A property you set to track how many bytes have been uploaded (for progress bars)
    - uploadTotal - A property you set to track the total bytes to upload
    - status - The current status of the upload ('idle', 'reading', 'uploading', 'uploaded', 'error', 'aborted')

    The upload adapter pattern:
    Your adapter class must implement:
    - upload() - Returns a Promise that resolves to an object with { default: url } (the URL where the uploaded file can be accessed)
    - abort() - (Optional) Cancels the upload

    Documentation:
    The official CKEditor 5 documentation for this is at:
    - https://ckeditor.com/docs/ckeditor5/latest/framework/deep-dive/upload-adapter.html
    - https://ckeditor.com/docs/ckeditor5/latest/api/module_upload_filerepository-FileLoader.html

    In our implementation, we:
    1. Get the File from loader.file promise
    2. Upload it via XHR
    3. Update loader.uploaded and loader.uploadTotal in the progress callback (for the progress bar)
    4. Resolve with { default: imageUrl } when complete

    The loader handles all the internal CKEditor state management - we just provide the upload mechanism.

    ...you don't need to set the status property. That's managed internally by CKEditor's FileLoader class based on the lifecycle
    of the Promise you return from  upload():

    - When you call upload(), CKEditor sets status to 'reading'
    - When your Promise is pending, it's 'uploading'
    - When your Promise resolves, it becomes 'uploaded'
    - When your Promise rejects, it becomes 'error'
    - If abort() is called, it becomes 'aborted'

    The status property is read-only from the adapter's perspective - you observe it, you don't set it. Your adapter's job is just to:
    1. Return a Promise from upload()
    2. Update uploaded and uploadTotal for progress (which we do)
    3. Optionally implement abort() (which we do)

    CKEditor handles all the status transitions automatically based on how your Promise behaves. So your current implementation is correct!
*/

// Custom upload adapter for Protopost
class ProtopostUploadAdapter {
    constructor(loader, postId, protopostLocation) {
        this.loader = loader;
        this.postId = postId;
        this.protopostLocation = protopostLocation;
    }

    upload() {
        return this.loader.file
            .then(file => new Promise((resolve, reject) => {
                this._initRequest(file);
                this._initListeners(resolve, reject, file);
                this._sendRequest(file);
            }));
    }

    abort() {
        if (this.xhr) {
            this.xhr.abort();
        }
    }

    _initRequest(file) {
        const xhr = this.xhr = new XMLHttpRequest();
        // Remove trailing slash from protopostLocation if present
        const baseUrl = this.protopostLocation.replace(/\/$/, '');
        xhr.open('POST', `${baseUrl}/protopost/upload-post-media/${this.postId}/${file.name}`, true);
        xhr.responseType = 'json';
    }

    _initListeners(resolve, reject, file) {
        const xhr = this.xhr;
        const loader = this.loader;
        const genericErrorText = `Couldn't upload file: ${file.name}.`;

        xhr.addEventListener('error', () => reject(genericErrorText));
        xhr.addEventListener('abort', () => reject());
        xhr.addEventListener('load', () => {
            const response = xhr.response;

            // console.log('Upload response:', response);

            if (!response || response.error) {
                return reject(response && response.error ? response.error.message : genericErrorText);
            }

            // The response should contain the PostMediaInfo with the path
            // Since the app uses a <base> tag for post media, we can use just the filename as a relative URL
            const imageUrl = response.path;

            console.log('Resolving with image URL:', imageUrl);

            // Dispatch custom event for upload completion
            console.log('CKEditor upload complete:', file.name);
            const event = new CustomEvent('ckeditorUploadComplete', {
                detail: {
                    fileName: file.name
                }
            });
            document.dispatchEvent(event);

            resolve({
                default: imageUrl
            });
        });

        if (xhr.upload) {
            xhr.upload.addEventListener('progress', evt => {
                if (evt.lengthComputable) {
                    loader.uploadTotal = evt.total;
                    loader.uploaded = evt.loaded;
                }
            });
        }
    }

    _sendRequest(file) {
        // Set Content-Type header
        if (file.type) {
            this.xhr.setRequestHeader('Content-Type', file.type);
        }

        // Send the file as binary data
        this.xhr.send(file);
    }
}

// Plugin to integrate the upload adapter
function ProtopostUploadAdapterPlugin(editor) {
    editor.plugins.get('FileRepository').createUploadAdapter = (loader) => {
        // Get postId and protopostLocation from global context
        const postId = globalThis.protopostCurrentPostId;
        const protopostLocation = globalThis.protopostLocation;

        if (!postId) {
            console.error('No protopostCurrentPostId set for image upload');
            return null;
        }

        return new ProtopostUploadAdapter(loader, postId, protopostLocation);
    };
}

// globalThis.bindCkEditor = ( containerId ) => {
//  //return InlineEditor.create(document.querySelector('#'+containerId), editorConfig);
//  return DecoupledEditor.create(document.querySelector('#'+containerId), editorConfig);
// }
globalThis.bindCkEditor = ( mainContainerId, toolbarContainerId ) => {
    // Add the upload adapter plugin to the config
    const configWithUpload = Object.assign({}, editorConfig, {
        extraPlugins: [ProtopostUploadAdapterPlugin]
    });

    const out = DecoupledEditor.create(document.querySelector('#'+mainContainerId), configWithUpload);
    out.then( editor => {
        // Wrap setData to track when we're loading content vs user insertions
        const originalSetData = editor.setData.bind(editor);
        editor.setData = (data) => {
            editor._protopostIsLoadingData = true;
            try {
                originalSetData(data);
            } finally {
                editor._protopostIsLoadingData = false;
            }
        };

        const toolbarContainer = document.querySelector('#'+toolbarContainerId);
        editor.ui.view.toolbar.element.style.width=0

        const recomputeToolbarWidth = function (event) {
          //console.log("recomputeToolbarWidth: " + event);
          editor.ui.view.toolbar.element.style.width=0

          if (toolbarContainer.clientWidth == 0) {
            setTimeout(recomputeToolbarWidth,50)
          }
          else {
            editor.ui.view.toolbar.element.style.width=toolbarContainer.clientWidth+"px";
            // This internal call triggers grouping logic immediately
            if (typeof editor.ui.view.toolbar._checkIfShouldGroup === 'function') {
              editor.ui.view.toolbar._checkIfShouldGroup();
            }
            console.log("toolbar width set to " + editor.ui.view.toolbar.element.style.width)
          }
        }
        window.addEventListener("resize", recomputeToolbarWidth);

        // console.log("document.readyState: " + document.readyState)
        if (document.readyState === "loading") {
          // The DOM is still loading, attach the event listener
          document.addEventListener("DOMContentLoaded", recomputeToolbarWidth);
        } else {
          // The DOM is already loaded, execute the function immediately
          recomputeToolbarWidth();
        }

        // Configure uploaded images to not define fixed width and height attributes,
        // and optionally use width: 100% as a style

        // 1. Extend schema to allow custom attributes
        editor.model.schema.extend('imageBlock', {
            allowAttributes: ['dataCkeditorInserted', 'dataCkeditorAspectRatio', 'dataCkEditorWidthPercent100']
        });

        // 2. Mark images inserted via UI (not from setData)
        editor.model.document.on('change:data', () => {

            const changes = editor.model.document.differ.getChanges();

            // console.log( "change:data changes:" )
            // console.log( changes )

            // Only mark images if we're NOT loading data
            if (!editor._protopostIsLoadingData) {
                editor.model.change(writer => {
                    for (const change of changes) {
                        // Mark any newly inserted images (from upload or URL toolbar)
                        if (change.type === 'insert' && change.name === 'imageBlock') {
                            const item = change.position.nodeAfter;
                            if (item && item.name === 'imageBlock' && !item.getAttribute('dataCkeditorInserted')) {
                                //console.log("marking with dataCkeditorInserted", item)
                                writer.setAttribute('dataCkeditorInserted', true, item);
                                const wp100 =
                                    (typeof globalThis.protopostExternalJsConfig === 'undefined' ||
                                     typeof globalThis.protopostExternalJsConfig.ckeditorLoadedImagesDefaultToWidth100Percent === 'undefined' ||
                                     globalThis.protopostExternalJsConfig.ckeditorLoadedImagesDefaultToWidth100Percent);
                                writer.setAttribute('dataCkEditorWidthPercent100', wp100, item);
                            }
                        }
                    }
                });
            }
        });


        // 3. Post-fixer to capture aspect ratio and remove width/height from UI-inserted images
        //    Will rerun until no more fixes have been made
        editor.model.document.registerPostFixer(writer => {

            const changes = editor.model.document.differ.getChanges();
            let wasFixed = false;

            for (const change of changes) {

                // console.log( "postfixer change:" )
                // console.log( change )

                if (change.type === 'attribute') {
                    if (change.attributeKey === 'width' || change.attributeKey === 'height') {

                        const item = change.range.start.nodeAfter || change.range.start.parent;
                        // Only process if marked as inserted AND not already processed (no aspect ratio yet)
                        if (item && item.name === 'imageBlock' &&
                            item.getAttribute('dataCkeditorInserted') &&
                            !item.hasAttribute('dataCkeditorAspectRatio')) {

                            // console.log("about to adjust width/height/style", item);

                            // Capture aspect ratio before removing dimensions
                            const width = item.getAttribute('width');
                            const height = item.getAttribute('height');

                            // console.log(`post-fixer: width: ${width}, height: ${height}`);

                            if (width && height) {
                                writer.setAttribute('dataCkeditorAspectRatio', `${width}/${height}`, item);
                                wasFixed = true;
                            }

                            // Remove width/height attributes only on first processing
                            if (item.hasAttribute('width')) {
                                writer.removeAttribute('width', item);
                                wasFixed = true;
                            }
                            if (item.hasAttribute('height')) {
                                writer.removeAttribute('height', item);
                                wasFixed = true;
                            }
                        }
                    }
                }
            }

            return wasFixed;
        });

        // 4. Add downcast conversion so it appears as data-ckeditor-inserted in HTML
        //    and set width: 100% style on the img element
        editor.conversion.for('downcast').add(dispatcher => {
            dispatcher.on('attribute:dataCkeditorInserted:imageBlock', (evt, data, conversionApi) => {

                // console.log("data", data);

                if (!conversionApi.consumable.consume(data.item, evt.name)) {
                    return;
                }

                const viewFigure = conversionApi.mapper.toViewElement(data.item);
                const viewWriter = conversionApi.writer;

                if (data.attributeNewValue) {
                    // Set width: 100% (by default, or if configured) and aspect-ratio styles on the img child element
                    const viewImg = viewFigure.getChild(0);

                    if (viewImg && viewImg.name === 'img') {
                        const wp100 = data.item.getAttribute('dataCkEditorWidthPercent100');
                        if (wp100) {
                            viewWriter.setStyle('width', '100%', viewImg);
                        }

                        // Set aspect ratio if we stored it
                        const aspectRatio = data.item.getAttribute('dataCkeditorAspectRatio');
                        if (aspectRatio) {
                            viewWriter.setStyle('aspect-ratio', aspectRatio, viewImg);
                        }
                    }
                }

                // viewWriter.setAttribute('data-ckeditor-insertion-completed', 'true', viewFigure);
            });
        });

        toolbarContainer.appendChild( editor.ui.view.toolbar.element );
    } ).catch( error => {
        console.error( error );
    } );
    return out
}
