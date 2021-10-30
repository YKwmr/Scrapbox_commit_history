scrapbox.PageMenu.addMenu({
		title: 'Download CSV',
		image: 'https://scrapbox.io/files/61584695c33cd9001ddebe1f.png'
	});
	
	scrapbox.PageMenu('Download CSV').addItem({
 	title: 'Commit Info',
 	image: 'https://scrapbox.io/files/61584695c33cd9001ddebe1f.png',
 	onClick: async () => {
			const project = scrapbox.Project.name;
			const title = scrapbox.Page.title;
			const id = scrapbox.Page.id;
			const res = await fetch(`/api/commits/${project}/${id}`)
			const {commits} = await res.json();
			var logs = [["commitId", "ParentId", "created", "userId", "operate", "lineId", "lineId2", "origText", "text"]];
	
			commits.forEach(commit => {
				commit.changes.forEach(change =>{
					if(Object.keys(change)[0].match(/_/)){
						let log = [];
						let d = new Date();
						d.setTime(commit.created * 1000);
						log.push(commit.id, commit.parentId, d, commit.userId);
      				//コミットID, 直前のコミットID, コミットの作成時刻, 編集者ID
						let ope = Object.keys(change)[0]; //operation = 操作
						let lineId = change[ope]; // lineId = 行ID
						let lineId2 = (ope === '_insert')?change.lines.id:'';
						let origText = (ope === '_insert')?'':"\""+change.lines.origText.replace(/\"/g, '\"\"')+"\"";
						let text = (ope === '_delete')?'':"\""+change.lines.text.replace(/\"/g, '\"\"')+"\"";
						log.push(ope, lineId, lineId2, origText, text);
						logs.push(log);
					}
				});
			});
			let data = logs.map((log) => log.join()).join('\r\n');
			let bom = new Uint8Array([0xef, 0xbb, 0xbf]);
			let blob = new Blob([bom, data], {'type':'text/csv'});
			let downloadLink = document.createElement('a');
 		downloadLink.download = `commit_${title}.csv`;
 		downloadLink.href = URL.createObjectURL(blob);
 		downloadLink.dataset.downloadurl = [
 			'text/plain',
 			downloadLink.download,
 			downloadLink.href
 		].join(':');
 		downloadLink.click();
		}
 });
 scrapbox.PageMenu('Download CSV').addItem({
 	title: 'Member Info',
		image: 'https://scrapbox.io/files/61584695c33cd9001ddebe1f.png',
 	onClick: async () => {
			const project = scrapbox.Project.name;
			const res = await fetch(`/api/projects/${project}`)
			const {users} = await res.json();
			var members =[["id", "name", "displayname", "email"]]
			
			users.forEach((user) => {
				members.push([user.id, user.name, user.displayName, user.email]);
			});
			let data = members.map((member) => member.join()).join('\r\n');
			let bom = new Uint8Array([0xef, 0xbb, 0xbf]);
			let blob = new Blob([bom, data], {'type':'text/csv'});
			let downloadLink = document.createElement('a');
			downloadLink.download = `member_${project}.csv`;
			downloadLink.href = URL.createObjectURL(blob);
			downloadLink.dataset.downloadurl = [
				'text/plain',
				downloadLink.download,
				downloadLink.href
			].join(':');
			downloadLink.click();
		}
 });
